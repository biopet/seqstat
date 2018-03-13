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

import nl.biopet.tools.seqstat.{
  BaseCounts,
  LengthHistogram,
  PositionalHistogram
}
import play.api.libs.json._

object Implicts {
  implicit val positionalHistogram: Reads[PositionalHistogram] =
    Json.reads[PositionalHistogram]
  implicit val charCounts: Reads[BaseCounts] = Json.reads[BaseCounts]
  implicit val LengthHistogram: Reads[LengthHistogram] =
    Json.reads[LengthHistogram]
  implicit val aggregationRead: Reads[AggregationRead] =
    Json.reads[AggregationRead]
  implicit val dataRead: Reads[DataRead] = Json.reads[DataRead]
  implicit val data: Reads[Data] = Json.reads[Data]
  implicit val aggregation: Reads[Aggregation] = Json.reads[Aggregation]
  implicit val readgroup: Reads[Readgroup] = Json.reads[Readgroup]
  implicit val library: Reads[Library] = Json.reads[Library]
  implicit val sample: Reads[Sample] = Json.reads[Sample]
  implicit val root: Reads[Root] = Json.reads[Root]
}
