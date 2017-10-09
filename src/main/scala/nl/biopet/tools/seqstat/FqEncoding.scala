package nl.biopet.tools.seqstat

object FqEncoding extends Enumeration {
  type FqEncoding = Value
  val Sanger = Value(33, "Sanger")
  val Solexa = Value(64, "Solexa")
  val Unknown = Value(0, "Unknown")
}
