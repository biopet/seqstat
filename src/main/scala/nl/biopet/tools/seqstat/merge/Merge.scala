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

import nl.biopet.tools.seqstat.SeqStat
import nl.biopet.tools.seqstat.schema.Root
import nl.biopet.utils.tool.ToolCommand

object Merge extends ToolCommand[Args] {
  def emptyArgs: Args = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    require(cmdArgs.outputFile.isDefined || cmdArgs.combineOutputFile.isDefined,
            "--outputFile and/or --combineOutputFile should be given")

    val root =
      Root.fromGroupStats(cmdArgs.inputFiles.flatMap(Root.fromFile(_).asStats))
    root.validate()
    cmdArgs.outputFile.foreach(file => root.writeFile(file))

    cmdArgs.combineOutputFile.foreach { file =>
      root.readgroups
        .map { case (_, rg) => rg.seqstat.asGroupStats }
        .reduce(_ + _)
        .toSchemaData
        .writeFile(file)
    }

    logger.info("Done")
  }

  def descriptionText: String =
    """
      |This module will merge seqstat files together and keep the sample/library/readgroup structure.
      |If required it's also possible to collapse this, the output file then des not have any sample/library/readgroup structure.
    """.stripMargin

  def manualText: String =
    s"""
      |When merging the files ${SeqStat.toolName} will validate the input files and the output files.
      |If aggregation values can not be regenerated the file is considered corrupt.
    """.stripMargin

  def exampleText: String =
    s"""
       |Merging multiple files:
       |${SeqStat.example("merge",
                          "-i",
                          "<seqstat file>",
                          "-i",
                          "<seqstat file>",
                          "-o",
                          "<output file>")}
       |
       |Merging multiple files as collapsed format:
       |${SeqStat.example("merge",
                          "-i",
                          "<seqstat file>",
                          "-i",
                          "<seqstat file>",
                          "--combinedOutputFile",
                          "<output file>")}
       |
       |Both output formats at the same time:
       |${SeqStat.example("merge",
                          "-i",
                          "<seqstat file>",
                          "-i",
                          "<seqstat file>",
                          "-o",
                          "<output file>",
                          "--combinedOutputFile",
                          "<output file>")}
       |
     """.stripMargin
}
