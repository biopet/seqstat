package nl.biopet.tools.seqstat

import java.io.File

import nl.biopet.utils.tool.AbstractOptParser

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {

  head(s"""
          |$cmdName - Summarize FastQ
      """.stripMargin)

  opt[File]('i', "fastq") required () unbounded () valueName "<fastq>" action {
    (x, c) =>
      c.copy(fastq = x)
  } validate { x =>
    if (x.exists) success else failure("FASTQ file not found")
  } text "FastQ file to generate stats from"
  opt[File]('o', "output") unbounded () valueName "<json>" action { (x, c) =>
    c.copy(outputJson = Some(x))
  } text "File to write output to, if not supplied output go to stdout"
}
