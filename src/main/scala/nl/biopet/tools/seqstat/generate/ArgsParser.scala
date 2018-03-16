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

import nl.biopet.utils.tool.{AbstractOptParser, ToolCommand}

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {

  opt[File]('i', "fastqR1")
    .required()
    .valueName("<fastq file>")
    .action((x, c) => c.copy(fastqR1 = x))
    .text("FastQ file to generate stats from")
  opt[File]('j', "fastqR2")
    .valueName("<fastq file>")
    .action((x, c) => c.copy(fastqR2 = Some(x)))
    .text("FastQ file to generate stats from")
  opt[File]('o', "output")
    .valueName("<json>")
    .required()
    .action((x, c) => c.copy(outputJson = x))
    .text("File to write output to, if not supplied output go to stdout")
  opt[String]("sample")
    .valueName("<name>")
    .required()
    .action((x, c) => c.copy(sample = x))
    .text("Sample name")
  opt[String]("library")
    .valueName("<name>")
    .required()
    .action((x, c) => c.copy(library = x))
    .text("Library name")
  opt[String]("readgroup")
    .valueName("<name>")
    .required()
    .action((x, c) => c.copy(readgroup = x))
    .text("Readgroup name")
}
