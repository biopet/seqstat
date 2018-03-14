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

package nl.biopet.tools.seqstat.generate

import java.io.File

import nl.biopet.tools.seqstat.schema.{AggregationRead, Root}
import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class GenerateTest extends ToolTest[Args] {
  def toolCommand: Generate.type = Generate

  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      Generate.main(Array())
    }
  }

  @Test
  def testSingle(): Unit = {
    val outputFile = File.createTempFile("seqstat.", ".json")
    outputFile.deleteOnExit()
    Generate.main(
      Array("-i",
            resourcePath("/R1.fq"),
            "-o",
            outputFile.getAbsolutePath,
            "--sample",
            "sampleName",
            "--library",
            "libName",
            "--readgroup",
            "rgName"))
    val root = Root.fromFile(outputFile)
    val seqstats = root.readgroups.map { case (_, v) => v.seqstat }.toList
    assert(seqstats.forall(!_.asGroupStats.isPaired))
    seqstats.size shouldBe 1
    seqstats.head.r2.isDefined shouldBe false
    seqstats.head.r1.aggregation shouldBe AggregationRead(
      4,
      4,
      "A",
      "!",
      List("Sanger", "Illumina 1.8+"),
      1,
      4)
  }

  @Test
  def testPaired(): Unit = {
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
        "sampleName",
        "--library",
        "libName",
        "--readgroup",
        "rgName"
      ))
    val root = Root.fromFile(outputFile)
    val seqstats = root.readgroups.map { case (_, v) => v.seqstat }.toList
    assert(seqstats.forall(_.asGroupStats.isPaired))
    seqstats.size shouldBe 1
    seqstats.head.r2.isDefined shouldBe true
    seqstats.head.r1.aggregation shouldBe AggregationRead(
      4,
      4,
      "A",
      "!",
      List("Sanger", "Illumina 1.8+"),
      1,
      4)
  }

}
