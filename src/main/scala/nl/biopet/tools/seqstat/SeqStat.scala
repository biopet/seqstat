/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.seqstat

import java.io.{File, PrintWriter}

import htsjdk.samtools.fastq.{FastqReader, FastqRecord}
import nl.biopet.utils.conversions
import nl.biopet.utils.tool.ToolCommand

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.collection.JavaConverters._

object SeqStat extends ToolCommand[Args] {
  def emptyArgs: Args = Args()
  def argsParser = new ArgsParser(this)

  var phredEncoding: FqEncoding.Value = FqEncoding.Sanger

  val reportValues = List(1, 10, 20, 30, 40, 50, 60)

  // the base quality for each position on the reads
  var quals: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer()
  var nucs: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer()

  // generate the baseHistogram and readHistogram
  var baseQualHistogram: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer()
  var readQualHistogram: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer()

  var nucleotideHistoMap: mutable.Map[Char, Long] = mutable.Map()
  private var baseQualHistoMap: mutable.Map[Int, Long] = mutable.Map(0 -> 0)
  private var readQualGTEHistoMap: mutable.Map[Int, Long] = mutable.Map(0 -> 0)

  /**
    *
    * @param quals Computed quality histogram [flat]
    */
  def detectPhredEncoding(quals: mutable.ArrayBuffer[Long]): Unit = {
    // substract 1 on high value, because we start from index 0
    val qualLowBoundary = quals.takeWhile(_ == 0).length
    val qualHighBoundary = quals.length - 1

    (qualLowBoundary < 59, qualHighBoundary > 74) match {
      case (false, true) => phredEncoding = FqEncoding.Solexa
      // TODO: check this later on
      // complex case, we cannot tell wheter this is a sanger or solexa
      // but since the qual_high_boundery exceeds any Sanger/Illumina1.8 quals, we can `assume` this is solexa
      // New @ 2016/01/26: Illumina X ten samples can contain Phred=Q42 (qual_high_boundery==75/K)
      case (true, true) => phredEncoding = FqEncoding.Solexa
      // this is definite a sanger sequence, the lower end is sanger only
      case (true, false) => phredEncoding = FqEncoding.Sanger
      case (_, _)        => phredEncoding = FqEncoding.Unknown
    }
  }

  // Setting up the internal storage for the statistics gathered for each read
  // 'nuc' are the nucleotides 'ACTGN', the max ASCII value for this is T, pre-init the ArrayBuffer to this value
  // as we don't expect the have other 'higher' numbered Nucleotides for now.
  case class BaseStat(qual: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer(),
                      nucs: mutable.ArrayBuffer[Long] =
                        mutable.ArrayBuffer.fill('T'.toInt + 1)(0))

  case class ReadStat(qual: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer(),
                      nucs: mutable.ArrayBuffer[Long] =
                        mutable.ArrayBuffer.fill('T'.toInt + 1)(0),
                      var withN: Long = 0L,
                      lengths: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer())

  val baseStats: mutable.ArrayBuffer[BaseStat] = mutable.ArrayBuffer()
  val readStats: ReadStat = ReadStat()

  var readLengthHistogram: mutable.Map[String, Long] = mutable.Map.empty

  /**
    * Compute the quality metric per read
    * Results are stored in baseStats and readStats
    *
    * @param record FastqRecord
    */
  def processRead(record: FastqRecord): Unit = {

    // Adjust/expand the length of baseStat case classes to the size of current
    // read if the current list is not long enough to store the data
    if (baseStats.length < record.length) {
      baseStats ++= mutable.ArrayBuffer.fill(record.length - baseStats.length)(
        BaseStat())
    }

    if (readStats.lengths.length <= record.length)
      readStats.lengths ++= mutable.ArrayBuffer.fill(
        record.length - readStats.lengths.length + 1)(0)

    val readQuality = record.getBaseQualityString
    val readNucleotides = record.getReadString

    readStats.lengths(record.length) += 1

    for (t <- 0 until record.length()) {
      if (baseStats(t).qual.length <= readQuality(t)) {
        baseStats(t).qual ++= mutable.ArrayBuffer.fill(
          readQuality(t).toInt - baseStats(t).qual.length + 1)(0)
      }
      baseStats(t).qual(readQuality(t)) += 1
      baseStats(t).nucs(readNucleotides(t)) += 1
      readStats.nucs(readNucleotides(t)) += 1
    }

    // implicit conversion to Int using foldLeft(0)
    val avgQual: Int = readQuality.sum / readQuality.length
    if (readStats.qual.length <= avgQual) {
      readStats.qual ++= mutable.ArrayBuffer.fill(
        avgQual - readStats.qual.length + 1)(0)
    }
    readStats.qual(avgQual) += 1
    if (readNucleotides.contains("N")) readStats.withN += 1L
  }

  /**
    * seqStat, the compute entrypoint where all statistics collection starts
    *
    * @param fqreader FastqReader
    * @return numReads - number of reads counted
    */
  def seqStat(fqreader: FastqReader): Long = {
    var numReads: Long = 0
    for (read <- fqreader.iterator.asScala) {
      processRead(read)
      numReads += 1
    }

    if (numReads % 1000000 == 0) {
      logger.info(s"Processed $numReads reads")
    }

    numReads
  }

  def summarize(): Unit = {
    // for every position to the max length of any read
    for (pos <- baseStats.indices) {
      // list all qualities at this particular position `pos`
      // fix the length of `quals`
      if (quals.length <= baseStats(pos).qual.length) {
        quals ++= mutable.ArrayBuffer.fill(
          baseStats(pos).qual.length - quals.length)(0)
      }
      if (nucs.length <= baseStats(pos).nucs.length) {
        nucs ++= mutable.ArrayBuffer.fill(
          baseStats(pos).nucs.length - nucs.length)(0)
      }
      // count into the quals
      baseStats(pos).qual.zipWithIndex foreach {
        case (value, index) => quals(index) += value
      }
      // count N into nucs
      baseStats(pos).nucs.zipWithIndex foreach {
        case (value, index) => nucs(index) += value
      }
    }
    detectPhredEncoding(quals)
    logger.debug(
      "Detected '" + phredEncoding.toString.toLowerCase + "' encoding in fastq file ...")

    nucleotideHistoMap = nucs.toList
      .foldLeft(mutable.Map[Char, Long]())(
        (output, nucleotideCount) =>
          output + (output.size.toChar -> nucleotideCount)
      )
      // ensure bases: `ACTGN` is always reported even having a zero count.
      // Other chars might be counted also, these are also reported
      .retain((nucleotide, count) =>
        count > 0 || "ACTGN".contains(nucleotide.toString))

    baseQualHistogram = quals.slice(phredEncoding.id, quals.size)
    baseQualHistogram ++= mutable.ArrayBuffer.fill(
      reportValues.max + 1 - baseQualHistogram.size)(0L)

    readQualHistogram =
      readStats.qual.slice(phredEncoding.id, readStats.qual.size)
    readQualHistogram ++= mutable.ArrayBuffer.fill(
      reportValues.max + 1 - readQualHistogram.size)(0L)

    readQualGTEHistoMap = readQualHistogram.indices
      .foldLeft(mutable.Map[Int, Long]())(
        (output, index) => {
          output + (output.keys.size -> readQualHistogram
            .slice(index, readQualHistogram.size)
            .sum)
        }
      )

  }

  def reportMap(fastqPath: File): Map[String, Any] = {
    Map(
      ("files",
       Map(
         ("fastq", Map(("path", fastqPath.getAbsolutePath)))
       )),
      ("stats",
       Map(
         ("bases",
          Map(
            ("num_total", nucleotideHistoMap.values.sum),
            ("num_qual", baseQualHistogram.toList),
            ("nucleotides", nucleotideHistoMap.toMap)
          )),
         ("reads",
          Map(
            ("num_with_n", readStats.withN),
            ("num_total", readStats.qual.sum),
            ("len_min", readStats.lengths.takeWhile(_ == 0).length),
            ("len_max", readStats.lengths.length - 1),
            ("num_avg_qual_gte", readQualGTEHistoMap.toMap),
            ("qual_encoding", phredEncoding.toString.toLowerCase),
            ("len_histogram", readStats.lengths.toList)
          ))
       ))
    )
  }

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    require(cmdArgs.fastq.exists(), "FASTQ file not found")
    logger.info("Start")

    seqStat(new FastqReader(cmdArgs.fastq))
    summarize()

    val report = reportMap(cmdArgs.fastq)

    cmdArgs.outputJson match {
      case Some(file) =>
        val writer = new PrintWriter(file)
        writer.println(conversions.mapToJson(report))
        writer.close()
      case _ => println(conversions.mapToJson(report))
    }

    logger.info("Done")
  }

  def descriptionText: String =
    """
      |SeqStats outputs several stats on a FASTQ file.
      |
      |Outputted stats:
      |
      |- Bases
      |   - Total number
      |   - Base qualities, with the number of bases having that quality
      |   - Number of each nucleotide
      |- Reads
      |   - Total number
      |   - Number of reads with 'N' bases
      |   - minimum length
      |   - maximum length
      |   - A histogram of the average base qualities
      |   - The quality encoding (Sanger, solexa etc.)
      |   - A histogram of the read lengths.
    """.stripMargin

  def manualText: String =
    """
      |By default stats are outputted to stdout in json format. If an output file
      |is specified it writes to the file in json format.
    """.stripMargin

  def exampleText: String =
    s"""
       |To run $toolName and save the output in a JSON file:
       |
       |${example("-i", "input.fastq", "-o", "output.json")}
       |
       |To run $toolName and wirte the output to stdout:
       |
       |${example("-i", "input.fastq")}
     """.stripMargin
}
