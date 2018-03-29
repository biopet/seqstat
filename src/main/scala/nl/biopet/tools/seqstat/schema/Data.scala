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

import java.io.File

import nl.biopet.tools.seqstat.{GroupStats, PositionalHistogram}
import nl.biopet.utils.io
import play.api.libs.json._
import nl.biopet.tools.seqstat.schema.Implicts._

case class Data(r1: DataRead, r2: Option[DataRead]) {
  def asGroupStats: GroupStats = {
    GroupStats(
      PositionalHistogram.fromMap(r1.nucleotidesPositionalHistogram),
      PositionalHistogram.fromMap(r1.qualityPositionalHistogram),
      r2.map(r =>
        PositionalHistogram.fromMap(r.nucleotidesPositionalHistogram)),
      r2.map(r => PositionalHistogram.fromMap(r.qualityPositionalHistogram))
    )
  }

  def validate(): Unit = {
    val groupStats = asGroupStats
    require(groupStats.r1seq.lengthHistogram.countsMap.map {
      case (k, v) => k.toString -> v
    } == r1.lengthHistogram)
    require(groupStats.r1qual.lengthHistogram.countsMap.map {
      case (k, v) => k.toString -> v
    } == r1.lengthHistogram)
    require(groupStats.r1seq.valueHistogram.countsMap.map {
      case (k, v) => k.toString -> v
    } == r1.nucleotidesHistogram)
    require(r1.aggregation == GroupStats.createAggregation(groupStats.r1seq,
                                                           groupStats.r1qual),
            "Aggregation R1 does not match")

    (r2, groupStats.r2seq, groupStats.r2qual) match {
      case (Some(r2Data), Some(r2seq), Some(r2qual)) =>
        require(r2seq.lengthHistogram.countsMap.map {
          case (k, v) => k.toString -> v
        } == r2Data.lengthHistogram)
        require(r2qual.lengthHistogram.countsMap.map {
          case (k, v) => k.toString -> v
        } == r2Data.lengthHistogram)
        require(r2seq.valueHistogram.countsMap.map {
          case (k, v) => k.toString -> v
        } == r2Data.nucleotidesHistogram)

        require(
          r2Data.aggregation == GroupStats.createAggregation(r2seq, r2qual),
          "Aggregation R2 does not match")
      case _ =>
    }
  }

  def toJson: JsValue = {
    Json.toJson(this)
  }

  def writeFile(file: File): Unit = {
    val json = toJson
    io.writeLinesToFile(file, Json.stringify(json) :: Nil)
  }
}
