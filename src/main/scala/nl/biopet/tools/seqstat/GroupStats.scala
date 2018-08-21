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

import htsjdk.samtools.fastq.FastqRecord
import nl.biopet.utils.Logging
import nl.biopet.utils.ngs.fastq.Encoding
import nl.biopet.utils.ngs.fastq.validation.checkMate

case class GroupStats(r1seq: PositionalHistogram,
                      r1qual: PositionalHistogram,
                      r2seq: Option[PositionalHistogram],
                      r2qual: Option[PositionalHistogram])
    extends Logging {

  def addFastqRecords(r1: FastqRecord, r2: Option[FastqRecord] = None): Unit = {
    r2.foreach(
      r2 =>
        require(checkMate(r1, r2),
                "R1 and R2 seems to be out of sync"))
    addFragment(r1.getReadString,
                r1.getBaseQualityString,
                r2.map(_.getReadString),
                r2.map(_.getBaseQualityString))
  }

  def addFragment(r1Seq: String,
                  r1Qual: String,
                  r2Seq: Option[String] = None,
                  r2Qual: Option[String] = None): Unit = {
    this.r1seq.addRead(r1Seq)
    this.r1qual.addRead(r1Qual)
    this.r2seq.foreach(
      _.addRead(r2Seq.getOrElse(
        throw new IllegalStateException("Object is for paired reads"))))
    this.r2qual.foreach(
      _.addRead(r2Qual.getOrElse(
        throw new IllegalStateException("Object is for paired reads"))))
  }

  def +(other: GroupStats): GroupStats = {
    val r2s = (this.r2seq, other.r2seq) match {
      case (Some(x), Some(y)) => Some(x += y)
      case (None, Some(y)) =>
        logger.warn("Merging paired and non-paired data")
        Some(y)
      case (Some(x), None) =>
        logger.warn("Merging paired and non-paired data")
        Some(x)
      case (None, None) => None
    }
    val r2q = (this.r2qual, other.r2qual) match {
      case (Some(x), Some(y)) => Some(x += y)
      case (None, Some(y))    => Some(y)
      case (Some(x), None)    => Some(x)
      case (None, None)       => None
    }
    GroupStats(this.r1seq += other.r1seq, this.r1qual += other.r1qual, r2s, r2q)
  }

  def isPaired: Boolean = r2seq.isDefined && r2qual.isDefined

  def toSchemaData: schema.Data = {
    val r1 = schema.DataRead(
      r1seq.toMap,
      r1qual.toMap,
      r1seq.lengthHistogram.countsMap.map { case (k, v) => k.toString -> v },
      r1seq.valueHistogram.countsMap.map { case (k, v)  => k.toString -> v },
      GroupStats.createAggregation(r1seq, r1qual)
    )
    val r2 = (r2seq, r2qual) match {
      case (Some(seq), Some(qual)) =>
        Some(
          schema.DataRead(
            seq.toMap,
            qual.toMap,
            seq.lengthHistogram.countsMap
              .map { case (k, v)                           => k.toString -> v },
            seq.valueHistogram.countsMap.map { case (k, v) => k.toString -> v },
            GroupStats.createAggregation(seq, qual)
          ))
      case _ => None
    }
    schema.Data(r1, r2)
  }
}

object GroupStats {

  def createAggregation(seq: PositionalHistogram,
                        qual: PositionalHistogram): schema.AggregationRead = {
    val lengtHist = seq.lengthHistogram.countsMap.filter {
      case (_, v) => v > 0
    }
    val nucHist = qual.valueHistogram.countsMap.filter {
      case (_, v) => v > 0
    }
    schema.AggregationRead(
      lengtHist.keys.max,
      lengtHist.keys.min,
      nucHist.keys.max.toString,
      nucHist.keys.min.toString,
      Encoding
        .possibleEncodings(nucHist.keys.min, nucHist.keys.max)
        .map(_.name),
      seq.totalReads,
      seq.totalBases
    )
  }

  def emptySingle: GroupStats = {
    GroupStats(PositionalHistogram.empty, PositionalHistogram.empty, None, None)
  }

  def emptyPaired: GroupStats = {
    GroupStats(PositionalHistogram.empty,
               PositionalHistogram.empty,
               Some(PositionalHistogram.empty),
               Some(PositionalHistogram.empty))
  }
}
