package code.year2024

import better.files.File
import code.Utils._

object Problem08 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem08.in"))
    val inputLines: List[String] = input.lines.toList

    val mapGrid: IndexedSeq[IndexedSeq[Char]] =
      inputLines.map(_.toIndexedSeq).toIndexedSeq

    val maxX = mapGrid.head.length
    val maxY = mapGrid.length

    val groupedAntenas: Map[Char, Seq[(Int, Int)]] =
      mapGrid.zipWithIndex
        .flatMap { case (row, y) =>
          row.zipWithIndex.collect { case (char, x) =>
            (char, x, y)
          }
        }
        .groupMap(_._1) { case (_, x, y) => (x, y) }
        .filterNot(_._1 == '.')

    val seenPart1: Array[Array[Boolean]] =
      Array.fill(maxY, maxX)(false)

    groupedAntenas.foreach { case (_, antenas) =>
      for {
        List(antena1, antena2) <- antenas.combinations(2).map(_.toList).toList
      } {
        val (x1, y1) = antena1
        val (x2, y2) = antena2

        val (x1a, y1a) = (x1, y1) + (x1 - x2, y1 - y2)
        val (x2a, y2a) = (x2, y2) + (x2 - x1, y2 - y1)

        Seq((x1a, y1a), (x2a, y2a)).foreach { case (x, y) =>
          if (x >= 0 && x < maxX && y >= 0 && y < maxY)
            seenPart1(y)(x) = true
        }
      }
    }

    val uniqueAntinodesPart1: Int =
      seenPart1.map(_.count(identity)).sum

    println("First Part: " + uniqueAntinodesPart1)

    val seenPart2: Array[Array[Boolean]] =
      Array.fill(maxY, maxX)(false)

    groupedAntenas.foreach { case (_, antenas) =>
      for {
        List(antena1, antena2) <- antenas.combinations(2).map(_.toList).toList
      } {
        val (x1, y1) = antena1
        val (x2, y2) = antena2

        LazyList
          .from(0)
          .map((x1 - x2, y1 - y2) * _ + (x1, y1))
          .takeWhile { case (x, y) => x >= 0 && x < maxX && y >= 0 && y < maxY }
          .foreach { case (x, y) => seenPart2(y)(x) = true }

        LazyList
          .from(0)
          .map((x2 - x1, y2 - y1) * _ + (x2, y2))
          .takeWhile { case (x, y) => x >= 0 && x < maxX && y >= 0 && y < maxY }
          .foreach { case (x, y) => seenPart2(y)(x) = true }
      }
    }

    val uniqueAntinodesPart2: Int =
      seenPart2.map(_.count(identity)).sum

    println("Second Part: " + uniqueAntinodesPart2)
  }

}
