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

import nl.biopet.utils.Histogram
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsValue}

import scala.collection.mutable
import scala.math.BigDecimal

class PositionalHistogram(
    _map: mutable.Map[Char, Histogram[Int]] = mutable.Map()) {

  protected val map: mutable.Map[Char, Histogram[Int]] = _map

  /** This will add a read to the histogram, this can be nucleotides of the qual string */
  def addRead(read: String): Unit = {
    read.zipWithIndex.foreach {
      case (char, idx) =>
        if (!map.contains(char)) map += char -> new Histogram[Int]()
        map(char).add(idx)
    }
  }

  /** With this method 2 [[PositionalHistogram]]s can be combined */
  def +=(other: PositionalHistogram): PositionalHistogram = {
    other.map.foreach {
      case (key, value) =>
        map.get(key) match {
          case Some(m) => map += key -> new Histogram((m += value).countsMap)
          case _       => map += key -> value
        }
    }
    this
  }

  /**
    * This returns a json object from this histogram
    *
    * {
    *   "<value>": [<count>]
    * }
    *
    */
  def toJson: JsObject = {
    JsObject(map.map {
      case (char, hist) =>
        val map = hist.countsMap
        val keys = 0 to map.keys.toList.sorted.max
        char.toString -> JsArray(
          keys.map(x => JsNumber(BigDecimal(map.getOrElse(x, 0L)))))
    }.toSeq)
  }

  /** This will return a a histogram of the length of all reads */
  def lengthHistogram: Histogram[Int] = {
    val keys = map
      .flatMap {
        case (_, hist) =>
          hist.countsMap.keySet
      }
      .toList
      .distinct
      .sorted
      .reverse
    keys.foldLeft(new Histogram[Int]()) {
      case (a, b) =>
        val total = a.total
        val current = map.flatMap { case (_, hist) => hist.get(b) }.sum
        a.addMulti(b + 1, current - total)
        a
    }
  }

  /** This will give a single histogram for the value, this will lose the positional information */
  def valueHistogram: Histogram[Char] = {
    new Histogram(map.map { case (k, v) => k -> v.total }.toMap)
  }

  /** Returns total read count */
  def totalReads: Long = {
    map.flatMap { case (_, hist) => hist.get(0) }.sum
  }

  /** Returns total base count */
  def totalBases: Long = {
    map.map { case (_, hist) => hist.total }.sum
  }

  def toMap: Map[String, Array[Long]] = {
    map.map {
      case (k, v) =>
        val content = v.countsMap
        k.toString -> (0 to v.countsMap.keys.max)
          .map(content.getOrElse(_, 0L))
          .toArray
    }.toMap
  }
}

object PositionalHistogram {

  /** Convert JsObject to a [[PositionalHistogram]] */
  def fromJson(content: JsObject): PositionalHistogram = {
    new PositionalHistogram(mutable.Map(content.value.map {
      case (char, hist) =>
        val key = char.headOption match {
          case Some(k) => k
          case _       => throw new IllegalStateException("Empty json key found")
        }
        if (char.length != 1)
          throw new IllegalStateException(
            "key can only have a single character")

        key -> new Histogram(
          hist
            .as[JsArray]
            .value
            .zipWithIndex
            .map {
              case (value, idx) =>
                idx -> value.as[Long]
            }
            .toMap)
    }.toSeq: _*))
  }

  /** Convert JsObject to a [[PositionalHistogram]] */
  def fromJson(content: JsValue): PositionalHistogram = {
    content.asOpt[JsObject] match {
      case Some(c) => fromJson(c)
      case _ =>
        throw new IllegalStateException(
          "Json value must be a object to be a PositionalHistogram")
    }
  }

  def fromMap(map: Map[String, Array[Long]]): PositionalHistogram = {
    new PositionalHistogram(mutable.Map() ++ map.map {
      case (k, v) =>
        k.head -> new Histogram(v.zipWithIndex.map {
          case (l, idx) => idx -> l
        }.toMap)
    })
  }

  def empty: PositionalHistogram = new PositionalHistogram()
}
