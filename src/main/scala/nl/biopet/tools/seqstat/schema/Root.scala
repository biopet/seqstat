package nl.biopet.tools.seqstat.schema

import java.io.File

import nl.biopet.tools.seqstat.{Group, Stats}
import nl.biopet.utils.io
import nl.biopet.utils.conversions
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
