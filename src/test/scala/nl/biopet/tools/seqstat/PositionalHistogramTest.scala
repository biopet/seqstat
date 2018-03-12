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
    histogram.lengthHistogram.countsMap shouldBe Map(1 -> 0, 2 -> 0, 3 -> 0, 4 -> 1)
    histogram.valueHistogram.countsMap shouldBe Map('A' -> 1, 'T' -> 1, 'G' -> 1, 'C' -> 1)
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
    histogram.lengthHistogram.countsMap shouldBe Map(1 -> 0, 2 -> 0, 3 -> 0, 4 -> 1, 5 -> 1)
    histogram.valueHistogram.countsMap shouldBe Map('A' -> 2, 'T' -> 3, 'G' -> 2, 'C' -> 2)
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
  def testJsonInput(): Unit ={
    val jsonString = """{"G":[0,0,1],"A":[1],"C":[0,0,0,1],"T":[0,1]}"""
    val json = Json.parse(jsonString)
    val histogram = PositionalHistogram.fromJson(json)
    histogram.totalBases shouldBe 4
    histogram.totalReads shouldBe 1
    histogram.lengthHistogram.countsMap shouldBe Map(1 -> 0, 2 -> 0, 3 -> 0, 4 -> 1)
    histogram.valueHistogram.countsMap shouldBe Map('A' -> 1, 'T' -> 1, 'G' -> 1, 'C' -> 1)
  }
}
