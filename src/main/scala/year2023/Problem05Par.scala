package year2023

import better.files.File

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq

object Problem05Par {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem05.in"))
    val lines: Seq[String] = input.lines.toSeq
    val (seedLine, otherLines) = lines.splitAt(1)
    val seedNumbers: Seq[Long] = seedLine.head.drop(6).trim.split(" ").map(_.toLong).toSeq
    val otherLinesFiltered: Seq[String] = otherLines.filter(_.nonEmpty)
    val mapLines: Seq[Int] =
      otherLinesFiltered.zipWithIndex.filter(_._1.contains(":")).map(_._2) :+
        otherLinesFiltered.size

    val allMaps: List[Seq[(Long, Long, Long)]] =
      mapLines
        .zip(mapLines.tail)
        .map { case (mapLineStart, mapLineEnd) =>
          val maps = otherLinesFiltered.slice(mapLineStart + 1, mapLineEnd)
          maps.map(_.split(" ").map(_.toLong).toSeq).map { case Seq(a, b, c) =>
            (a, b, c)
          }
        }
        .toList

    @tailrec
    def processElement(remainingMaps: List[Seq[(Long, Long, Long)]], elem: Long): Long = {
      remainingMaps match {
        case Nil => elem
        case headMap :: nextMaps =>
          val mapResult: Long =
            headMap
              .find { case (_, sourceRange, rangeLen) =>
                elem >= sourceRange && elem < sourceRange + rangeLen
              }
              .map { case (target, sourceRange, _) =>
                target + elem - sourceRange
              }
              .getOrElse(elem)
          processElement(nextMaps, mapResult)
      }
    }

    println(
      "First Part: " + seedNumbers.map(processElement(allMaps, _)).min
    )

    val allSeeds2: Seq[Seq[Long]] = seedNumbers.grouped(2).toSeq
    val initial = System.currentTimeMillis()

    val minValues: ParSeq[Long] =
      allSeeds2.par.map { case Seq(a, b) =>
        var min: Long = Long.MaxValue
        var n: Long = a
        while (n < a + b) {
          val result = processElement(allMaps, n)
          if (result < min)
            min = result
          n += 1
        }
        min
      }

    println(
      "Second Part: " + minValues.min
    )
    println("Total time: " + (System.currentTimeMillis() - initial) + "ms")
  }

}
