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

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test
import play.api.libs.json.Json

class PositionalHistogramTest extends BiopetTest {
  @Test
  def testAddRead(): Unit = {
    val histogram = new PositionalHistogram()
    histogram.addRead("ATGC")
    histogram.totalBases shouldBe 4
    histogram.totalReads shouldBe 1
    histogram.lengthHistogram.countsMap shouldBe Map(1 -> 0,
                                                     2 -> 0,
                                                     3 -> 0,
                                                     4 -> 1)
    histogram.valueHistogram.countsMap shouldBe Map('A' -> 1,
                                                    'T' -> 1,
                                                    'G' -> 1,
                                                    'C' -> 1)
  }

  @Test
  def testGather(): Unit = {
    val histogram = new PositionalHistogram()
    val histogram2 = new PositionalHistogram()
    histogram.addRead("AATT")
    histogram2.addRead("GGCCT")
    histogram += histogram2
    histogram.totalBases shouldBe 9
    histogram.totalReads shouldBe 2
    histogram.lengthHistogram.countsMap shouldBe Map(1 -> 0,
                                                     2 -> 0,
                                                     3 -> 0,
                                                     4 -> 1,
                                                     5 -> 1)
    histogram.valueHistogram.countsMap shouldBe Map('A' -> 2,
                                                    'T' -> 3,
                                                    'G' -> 2,
                                                    'C' -> 2)
  }

  @Test
  def testJsonOutput(): Unit = {
    val histogram = new PositionalHistogram()
    histogram.addRead("ATGC")

    val jsonString = """{"G":[0,0,1],"A":[1],"C":[0,0,0,1],"T":[0,1]}"""
    val json = Json.parse(jsonString)

    histogram.toJson shouldBe json
  }

  @Test
  def testJsonInput(): Unit = {
    val jsonString = """{"G":[0,0,1],"A":[1],"C":[0,0,0,1],"T":[0,1]}"""
    val json = Json.parse(jsonString)
    val histogram = PositionalHistogram.fromJson(json)
    histogram.totalBases shouldBe 4
    histogram.totalReads shouldBe 1
    histogram.lengthHistogram.countsMap shouldBe Map(1 -> 0,
                                                     2 -> 0,
                                                     3 -> 0,
                                                     4 -> 1)
    histogram.valueHistogram.countsMap shouldBe Map('A' -> 1,
                                                    'T' -> 1,
                                                    'G' -> 1,
                                                    'C' -> 1)
  }
}
