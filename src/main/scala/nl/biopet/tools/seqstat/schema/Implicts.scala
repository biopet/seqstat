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

import play.api.libs.json._

object Implicts {
  implicit val aggregationReadReads: Reads[AggregationRead] =
    Json.reads[AggregationRead]
  implicit val dataReadReads: Reads[DataRead] = Json.reads[DataRead]
  implicit val dataReads: Reads[Data] = Json.reads[Data]
  implicit val aggregationReads: Reads[Aggregation] = Json.reads[Aggregation]
  implicit val readgroupReads: Reads[Readgroup] = Json.reads[Readgroup]
  implicit val libraryReads: Reads[Library] = Json.reads[Library]
  implicit val sampleReads: Reads[Sample] = Json.reads[Sample]
  implicit val rootReads: Reads[Root] = Json.reads[Root]

  implicit val aggregationReadWrites: Writes[AggregationRead] =
    Json.writes[AggregationRead]
  implicit val dataReadWrites: Writes[DataRead] = Json.writes[DataRead]
  implicit val dataWrites: Writes[Data] = Json.writes[Data]
  implicit val aggregationWrites: Writes[Aggregation] = Json.writes[Aggregation]
  implicit val readgroupWrites: Writes[Readgroup] = Json.writes[Readgroup]
  implicit val libraryWrites: Writes[Library] = Json.writes[Library]
  implicit val sampleWrites: Writes[Sample] = Json.writes[Sample]
  implicit val rootWrites: Writes[Root] = Json.writes[Root]

}
