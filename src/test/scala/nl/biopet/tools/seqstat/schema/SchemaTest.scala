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

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test
import play.api.libs.json._

class SchemaTest extends BiopetTest {
  @Test
  def testSchema(): Unit = {

    val json: JsValue = Json.parse(
      """{
        |"samples": {
        | "sample1": {
        |   "libraries": {
        |     "lib1": {
        |       "readgroups": {
        |         "rg1": {
        |           "seqstat": {"r1": {
        |             "nucleotidesHistogram": {"5": 5},
        |             "qualityPositionalHistogram": {"A": [2,4,6]},
        |             "nucleotidesPositionalHistogram": {"A": [2,4,6]},
        |             "lengthHistogram": {"5": 5},
        |             "aggregation": {"maxLength": 4, "minLength": 4, "maxQuality": "!", "minQuality": "J", "qualityEncoding": ["Sanger"], "basesTotal": 16, "readsTotal": 4} }
        |           }
        |         }
        |       }
        |     }
        |   }
        | }
        |}
        |}""".stripMargin
    )

    Root.fromJson(json)
  }
}
