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

import htsjdk.samtools.fastq.FastqReader
import nl.biopet.tools.seqstat.{Group, GroupStats}
import nl.biopet.utils.tool.ToolCommand
import scala.collection.JavaConversions._

object Generate extends ToolCommand[Args] {
  def emptyArgs: Args = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)
    val group = Group(cmdArgs.sample, cmdArgs.library, cmdArgs.readgroup)
    val stats = cmdArgs.fastqR2 match {
      case Some(r2) => GroupStats.emptyPaired
      case _        => GroupStats.emptySingle
    }

    val readerR1 = new FastqReader(cmdArgs.fastqR1)

    cmdArgs.fastqR2 match {
      case Some(r2File) =>
        val readerR2 = new FastqReader(r2File)
        readerR1.iterator().zip(readerR2.iterator()).foreach {
          case (r1, r2) => stats.addFastqRecords(r1, Some(r2))
        }
        if (readerR1.hasNext)
          throw new IllegalStateException("Out of sync, r1 has reads left")
        if (readerR2.hasNext)
          throw new IllegalStateException("Out of sync, r2 has reads left")
        readerR2.close()
      case _ => readerR1.iterator().foreach(stats.addFastqRecords(_, None))
    }
    readerR1.close()

    //TODO: Write file

    logger.info("Done")
  }

  def descriptionText: String =
    """
      |
    """.stripMargin

  def manualText: String =
    """
      |
    """.stripMargin

  def exampleText: String =
    s"""
       |
     """.stripMargin

}
