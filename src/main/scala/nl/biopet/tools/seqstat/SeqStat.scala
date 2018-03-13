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

import nl.biopet.tools.seqstat.generate.Generate
import nl.biopet.tools.seqstat.merge.Merge
import nl.biopet.utils.tool.ToolCommand

object SeqStat extends ToolCommand[Args] {
  def emptyArgs: Args = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    cmdArgs.mode.map(_.toLowerCase) match {
      case Some(mode) =>
        modes.find(_.toolName.toLowerCase == toolName) match {
          case Some(tool) => tool.main(cmdArgs.toolArgs)
          case _ =>
            logger.error(s"Tool '$toolName' not found")
            printToolList()
        }
      case _ => printToolList()
    }
  }

  def printToolList(): Unit = {
    argsParser.usage.split("\n").foreach(logger.info)
    logger.info("")
    printToolList("Modes", modes)
  }

  def printToolList(title: String, tools: List[ToolCommand[_]]): Unit = {
    logger.info(s"** $title **")
    tools
      .map(_.toolName)
      .grouped(6)
      .map(x => x.mkString(", "))
      .foreach(logger.info)
    logger.info("")
  }

  def modes: List[ToolCommand[_]] = List(
    Generate,
    Merge
  )

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
