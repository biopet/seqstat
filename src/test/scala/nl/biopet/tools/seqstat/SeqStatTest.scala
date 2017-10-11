package nl.biopet.tools.seqstat

import java.io.File
import java.util

import org.mockito.Mockito.when
import htsjdk.samtools.fastq.{FastqReader, FastqRecord}
import nl.biopet.test.BiopetTest
import org.mockito.stubbing.OngoingStubbing
import org.scalatest.mock.MockitoSugar
import org.testng.annotations.{DataProvider, Test}

import scala.collection.JavaConverters._

class SeqStatTest extends BiopetTest with MockitoSugar {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      SeqStat.main(Array())
    }
  }

  // Helper functions to create iterator over FastqRecords given its IDs as Ints
  // Record with 'A' and Qual==39 (SangerEncoding)
  private def recordsOver(ids: String*): java.util.Iterator[FastqRecord] =
  ids
    .map(x => new FastqRecord(x, "ACGTN", "", "HIBC!"))
    .toIterator
    .asJava

  @DataProvider(name = "mockReaderProvider")
  def mockReaderProvider() =
    Array(
      Array(mock[FastqReader])
    )

  @Test(dataProvider = "mockReaderProvider", groups = Array("sanger"), singleThreaded = true)
  def testDefault(fqMock: FastqReader): OngoingStubbing[util.Iterator[FastqRecord]] = {
    when(fqMock.iterator) thenReturn recordsOver("1", "2", "3")
  }

  @Test(dataProvider = "mockReaderProvider", groups = Array("read"), singleThreaded = true)
  def testSeqCountReads(fqMock: FastqReader): Unit = {
    when(fqMock.iterator) thenReturn recordsOver("1", "2", "3", "4", "5")

    val seqstat = SeqStat
    val numReads = seqstat.seqStat(fqMock)
    numReads shouldBe 5
  }

  @Test(dataProvider = "mockReaderProvider",
    groups = Array("phredscore"),
    singleThreaded = true,
    dependsOnGroups = Array("read"))
  def testEncodingDetectionSanger(fqMock: FastqReader): Unit = {

    val seqstat = SeqStat
    seqstat.summarize()

    seqstat.phredEncoding shouldBe FqEncoding.Sanger
  }

  @Test(dataProvider = "mockReaderProvider",
    groups = Array("nucleocount"),
    singleThreaded = true,
    dependsOnGroups = Array("phredscore"))
  def testEncodingNucleotideCount(fqMock: FastqReader): Unit = {

    SeqStat.nucleotideHistoMap('N') shouldEqual 5
    SeqStat.nucleotideHistoMap('A') shouldEqual 5
    SeqStat.nucleotideHistoMap('C') shouldEqual 5
    SeqStat.nucleotideHistoMap('T') shouldEqual 5
    SeqStat.nucleotideHistoMap('G') shouldEqual 5
  }

  @Test(dataProvider = "mockReaderProvider",
    groups = Array("basehistogram"),
    singleThreaded = true,
    dependsOnGroups = Array("nucleocount"))
  def testEncodingBaseHistogram(fqMock: FastqReader): Unit = {

    SeqStat.baseQualHistogram(40) shouldEqual 5
    SeqStat.baseQualHistogram(39) shouldEqual 5
    SeqStat.baseQualHistogram(34) shouldEqual 5
    SeqStat.baseQualHistogram(33) shouldEqual 5
    SeqStat.baseQualHistogram.head shouldEqual 5
  }

  @Test(dataProvider = "mockReaderProvider",
    groups = Array("report"),
    singleThreaded = true,
    dependsOnGroups = Array("basehistogram"))
  def testReportOutputScheme(fqMock: FastqReader): Unit = {
    when(fqMock.getFile) thenReturn new File("/tmp/test.fq")
    when(fqMock.iterator) thenReturn recordsOver("1", "2", "3", "4", "5")
    val seqstat = SeqStat
    seqstat.seqStat(fqMock)
    seqstat.summarize()

    val report = seqstat.reportMap(fqMock.getFile)
    report should contain key "files"
    report should contain key "stats"

  }

  @Test(dataProvider = "mockReaderProvider",
    groups = Array("check_readstats"),
    singleThreaded = true,
    dependsOnGroups = Array("report"))
  def testReadStatsObject(fqMock: FastqReader): Unit = {
    when(fqMock.getFile) thenReturn new File("/tmp/test.fq")
    when(fqMock.iterator) thenReturn recordsOver("1", "2", "3", "4", "5")
    val seqstat = SeqStat

    // the histogram should store the lenght==0 value also, for example sequence length 5 is size 6.
    // please note that we already loaded the dataset twice in seqstat. (seqstat.Seqstat is called 2 times in previous steps)
    seqstat.readStats.lengths(5) shouldBe 10
    seqstat.readStats.lengths.length shouldBe 6

    seqstat.readStats.nucs.sum shouldBe 50
    seqstat.readStats.withN shouldBe 10
  }
}
