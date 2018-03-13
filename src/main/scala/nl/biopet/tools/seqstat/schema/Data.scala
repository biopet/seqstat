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

package nl.biopet.tools.seqstat.schema

import nl.biopet.tools.seqstat.{GroupStats, PositionalHistogram}

case class Data(r1: DataRead, r2: Option[DataRead]) {
  def asGroupStats: GroupStats = {
    GroupStats(
      PositionalHistogram.fromMap(r1.nucleotides_positional_histogram),
      PositionalHistogram.fromMap(r1.quality_positional_histogram),
      r2.map(r =>
        PositionalHistogram.fromMap(r.nucleotides_positional_histogram)),
      r2.map(r => PositionalHistogram.fromMap(r.quality_positional_histogram))
    )
  }

  def validate(): Unit = {
    val groupStats = asGroupStats
    require(groupStats.r1seq.lengthHistogram.countsMap.map {
      case (k, v) => k.toString -> v
    } == r1.length_histogram)
    require(groupStats.r1qual.lengthHistogram.countsMap.map {
      case (k, v) => k.toString -> v
    } == r1.length_histogram)
    require(groupStats.r1seq.valueHistogram.countsMap.map {
      case (k, v) => k.toString -> v
    } == r1.nucleotides_histogram)
    require(r1.aggregation == GroupStats.createAggregation(groupStats.r1seq,
                                                           groupStats.r1qual),
            "Aggregation R1 does not match")

    (r2, groupStats.r2seq, groupStats.r2qual) match {
      case (Some(r2Data), Some(r2seq), Some(r2qual)) =>
        require(r2seq.lengthHistogram.countsMap.map {
          case (k, v) => k.toString -> v
        } == r2Data.length_histogram)
        require(r2qual.lengthHistogram.countsMap.map {
          case (k, v) => k.toString -> v
        } == r2Data.length_histogram)
        require(r2seq.valueHistogram.countsMap.map {
          case (k, v) => k.toString -> v
        } == r2Data.nucleotides_histogram)

        require(
          r2Data.aggregation == GroupStats.createAggregation(r2seq, r2qual),
          "Aggregation R2 does not match")
      case _ =>
    }
  }
}
