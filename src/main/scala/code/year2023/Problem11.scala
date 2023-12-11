package code.year2023

import better.files.File

object Problem11 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem11.in"))
    val galaxyMap: IndexedSeq[IndexedSeq[Char]] =
      input.lines.toIndexedSeq.map(_.toCharArray.toIndexedSeq)

    val emptyLines: IndexedSeq[Int] =
      galaxyMap.map(_.forall(_ == '.')).zipWithIndex.filter(_._1).map(_._2)
    val emptyColumns: IndexedSeq[Int] =
      galaxyMap.transpose.map(_.forall(_ == '.')).zipWithIndex.filter(_._1).map(_._2)

    val allGalaxies: Seq[(Int, Int)] =
      galaxyMap.zipWithIndex.flatMap { case (line, lineIndex) =>
        line.zipWithIndex.flatMap {
          case ('#', columnIndex) => Seq((lineIndex, columnIndex))
          case _                  => Seq.empty
        }
      }

    val combinations: Seq[Seq[(Int, Int)]] =
      allGalaxies.combinations(2).toSeq

    def getDistances(distanceMult: Long): Long =
      combinations.map { case Seq((line1, column1), (line2, column2)) =>
        val distance = Math.abs(line1 - line2) + Math.abs(column1 - column2)
        val emptyLinesBetween: Int =
          emptyLines.count(line => line > Math.min(line1, line2) && line < Math.max(line1, line2))
        val emptyColumnsBetween: Int =
          emptyColumns.count(column =>
            column > Math.min(column1, column2) && column < Math.max(column1, column2)
          )
        distance.toLong +
          emptyLinesBetween * (distanceMult - 1) +
          emptyColumnsBetween * (distanceMult - 1)
      }.sum

    println("First Part: " + getDistances(2))
    println("Second Part: " + getDistances(1000000))
  }

}
