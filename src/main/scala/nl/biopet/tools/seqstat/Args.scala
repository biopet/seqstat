package nl.biopet.tools.seqstat

import java.io.File

case class Args(fastq: File = null, outputJson: Option[File] = None)
