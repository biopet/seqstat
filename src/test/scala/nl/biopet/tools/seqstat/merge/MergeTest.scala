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

package nl.biopet.tools.seqstat.merge

import java.io.File

import nl.biopet.tools.seqstat.generate.Generate
import nl.biopet.tools.seqstat.schema.{AggregationRead, Root}
import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class MergeTest extends ToolTest[Args] {
  def toolCommand: Merge.type = Merge
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      Merge.main(Array())
    }
  }

  def generateTestFile(sample: String,
                       library: String,
                       readgroup: String): File = {
    val outputFile = File.createTempFile("seqstat.", ".json")
    outputFile.deleteOnExit()
    Generate.main(
      Array(
        "-i",
        resourcePath("/R1.fq"),
        "-j",
        resourcePath("/R2.fq"),
        "-o",
        outputFile.getAbsolutePath,
        "--sample",
        sample,
        "--library",
        library,
        "--readgroup",
        readgroup
      ))
    outputFile
  }

  @Test
  def testMerge(): Unit = {
    val inputFile1 = generateTestFile("sampleName", "libraryName", "rgName")
    val outputFile = File.createTempFile("seqstat.", ".json")
    outputFile.deleteOnExit()

    Merge.main(
      Array("-i",
            inputFile1.getAbsolutePath,
            "-i",
            inputFile1.getAbsolutePath,
            "-o",
            outputFile.getAbsolutePath))

    val root = Root.fromFile(outputFile)
    val seqstats = root.readgroups.map { case (_, v) => v.seqstat }.toList
    assert(seqstats.forall(_.asGroupStats.isPaired))
    seqstats.size shouldBe 1
    seqstats.headOption.map(_.r2.isDefined) shouldBe Some(true)
    seqstats.headOption.map(_.r1.aggregation) shouldBe Some(
      AggregationRead(4, 4, "A", "!", List("Sanger", "Illumina 1.8+"), 2, 8))
  }

  @Test
  def testMultiMerge(): Unit = {
    val inputFile1 = generateTestFile("sampleName1", "libraryName", "rgName")
    val inputFile2 = generateTestFile("sampleName1", "libraryName", "rgName")
    val inputFile3 = generateTestFile("sampleName2", "libraryName", "rgName")
    val outputFile = File.createTempFile("seqstat.", ".json")
    outputFile.deleteOnExit()
    val combinedOutputFile = File.createTempFile("seqstat.", ".json")
    combinedOutputFile.deleteOnExit()

    Merge.main(
      Array(
        "-i",
        inputFile1.getAbsolutePath,
        "-i",
        inputFile2.getAbsolutePath,
        "-i",
        inputFile3.getAbsolutePath,
        "-o",
        outputFile.getAbsolutePath,
        "--combinedOutputFile",
        combinedOutputFile.getAbsolutePath
      ))

    val root = Root.fromFile(outputFile)
    val seqstats = root.readgroups.map { case (_, v) => v.seqstat }.toList
    assert(seqstats.forall(_.asGroupStats.isPaired))
    root.samples.size shouldBe 2
    seqstats.size shouldBe 2
    seqstats.headOption.map(_.r2.isDefined) shouldBe Some(true)
    seqstats.headOption.map(_.r1.aggregation) shouldBe Some(
      AggregationRead(4, 4, "A", "!", List("Sanger", "Illumina 1.8+"), 2, 8))
  }
}
