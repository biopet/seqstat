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

package object schema {
  case class Root(samples: Map[String, Sample], seqstat: Option[Aggregation]) {
    def readgroups: Map[Group, Readgroup] = samples.flatMap {
      case (sample, s) =>
        s.libraries.flatMap {
          case (library, l) =>
            l.readgroups.map {
              case (readgroup, r) => Group(sample, library, readgroup) -> r
            }
        }
    }
  }
  case class Sample(libraries: Map[String, Library],
                    seqstat: Option[Aggregation]) {}
  case class Library(readgroups: Map[String, Readgroup],
                     seqstat: Option[Aggregation]) {}
  case class Readgroup(seqstat: Data)
  case class Aggregation(R1: AggregationRead, R2: Option[AggregationRead])
  case class AggregationRead(max_len: Int,
                             min_len: Int,
                             qual_encoding: String,
                             read_total: Long,
                             bases_total: Long)
  case class DataRead(
      nucleotides_positional_histogram: Map[String, Array[Long]],
      quality_positional_histogram: Map[String, Array[Long]],
      length_histogram: Map[String, Long],
      nucleotides_histogram: Map[String, Long],
      aggregation: AggregationRead)
}
