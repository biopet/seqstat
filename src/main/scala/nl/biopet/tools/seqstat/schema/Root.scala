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

import nl.biopet.tools.seqstat.{Group, Stats}
import nl.biopet.utils.{conversions, io}
import play.api.libs.json._
import nl.biopet.tools.seqstat.schema.Implicts._

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

  def toJson: JsValue = {
    Json.toJson(this)
  }

  def writeFile(file: File): Unit = {
    val json = toJson
    io.writeLinesToFile(file, Json.stringify(json) :: Nil)
  }

  def asStats: List[Stats] = {
    readgroups.map {
      case (group, readgroup) => Stats(group, readgroup.seqstat.asGroupStats)
    }.toList
  }

  def validate(): Unit = {
    readgroups.foreach { case (_, rg) => rg.seqstat.validate() }
  }
}

object Root {
  def fromFile(file: File): Root = {
    fromJson(conversions.fileToJson(file))
  }

  def fromJson(json: JsValue): Root = {
    Json.fromJson[Root](json) match {
      case JsSuccess(r: Root, path: JsPath) => r
      case e: JsError =>
        throw new IllegalStateException(e.errors.mkString("\n"))
    }
  }

  def fromGroupStats(groups: List[Stats]): Root = {
    Root(
      groups.groupBy(_.group.sample).map {
        case (sampleId, sampleGroups) =>
          sampleId -> Sample(
            sampleGroups.groupBy(_.group.library).map {
              case (libraryId, libraryGroups) =>
                libraryId -> Library(
                  libraryGroups.groupBy(_.group.readgroup).map {
                    case (readgroupId, readgroups) =>
                      readgroupId -> Readgroup(
                        readgroups.map(_.stats).reduce(_ + _).toSchemaData)
                  },
                  None
                )
            },
            None
          )
      },
      None
    )
  }
}
