package code.year2024

import better.files.File

import scala.annotation.tailrec

object Problem04 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem04.in"))
    val grid: IndexedSeq[IndexedSeq[Char]] = input.lines.toIndexedSeq.map(_.toIndexedSeq)

    val maxX = grid.head.length
    val maxY = grid.length

    {
      val numberN: IndexedSeq[IndexedSeq[Int]] =
        grid.map {
          _.map {
            case 'X' => 0
            case 'M' => 1
            case 'A' => 2
            case 'S' => 3
            case _   => -1
          }
        }

      val xPos: IndexedSeq[(Int, Int)] =
        numberN.zipWithIndex.flatMap { case (row, y) =>
          row.zipWithIndex.collect { case (0, x) =>
            (x, y)
          }
        }

      @tailrec
      def checkXMAS(ix: Int, iy: Int, dx: Int, dy: Int, index: Int): Boolean =
        if (index == 4)
          true
        else
          ix >= 0 && ix < maxX && iy >= 0 && iy < maxY &&
            numberN(iy)(ix) == index && checkXMAS(ix + dx, iy + dy, dx, dy, index + 1)

      val total = {
        for {
          (ix, iy) <- xPos
          dx <- -1 to 1
          dy <- -1 to 1
          if dx != 0 || dy != 0
          if checkXMAS(ix + dx, iy + dy, dx, dy, 1)
        } yield {
          (ix, iy, dx, dy)
        }
      }.size

      println("First Part: " + total)
    }
    {
      val numberN: IndexedSeq[IndexedSeq[Int]] =
        grid.map {
          _.map {
            case 'A' => 0
            case 'M' => 1
            case 'S' => 2
            case _   => -1
          }
        }

      val aPos: IndexedSeq[(Int, Int)] =
        numberN.zipWithIndex.flatMap { case (row, y) =>
          row.zipWithIndex.collect {
            case (0, x) if x > 0 && x < maxX - 1 && y > 0 && y < maxY - 1 =>
              (x, y)
          }
        }

      def checkMASCross(ix: Int, iy: Int, checkList: List[(Int, Int)]): Boolean = {
        checkList match {
          case List((m1x, m1y), (m2x, m2y), (s1x, s1y), (s2x, s2y)) =>
            numberN(iy + m1y)(ix + m1x) == 1 &&
              numberN(iy + m2y)(ix + m2x) == 1 &&
              numberN(iy + s1y)(ix + s1x) == 2 &&
              numberN(iy + s2y)(ix + s2x) == 2
          case _ =>
            false
        }
      }

      val masPos1: List[(Int, Int)] =
        List(
          (-1, -1),
          (-1, 1),
          (1, 1),
          (1, -1),
        )

      val masPos2: List[List[(Int, Int)]] =
        (masPos1 ++ masPos1).tails.take(4).toList.map(_.take(4))

      val total = {
        for {
          (ix, iy) <- aPos
          checkList <- masPos2
          if checkMASCross(ix, iy, checkList)
        } yield {
          (ix, iy, checkList)
        }
      }.size

      println("Second Part: " + total)
    }
  }

}
