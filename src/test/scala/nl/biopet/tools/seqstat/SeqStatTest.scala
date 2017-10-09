package nl.biopet.tools.seqstat

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class SeqStatTest extends BiopetTest {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      SeqStat.main(Array())
    }
  }
}
