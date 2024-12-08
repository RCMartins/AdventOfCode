package code.year2022

import better.files.File
import code.Utils._

object Problem09 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem09.in"))
    val lines =
      input.lines.toIndexedSeq.map { case s"$c $n" => (c.head, n.toInt) }.flatMap { case (c, n) =>
        (1 to n).map(_ => c)
      }

    def moveDir(move: Char): (Int, Int) =
      move match {
        case 'U' => (0, -1)
        case 'D' => (0, 1)
        case 'L' => (-1, 0)
        case 'R' => (1, 0)
      }

    val maxSize = 700

    def simulate(nKnots: Int): Int = {
      val map = Array.fill(maxSize, maxSize)(false)
      val start = maxSize / 2
      map(start)(start) = true

      def updateKnots(knots: List[(Int, Int)], before: (Int, Int)): List[(Int, Int)] =
        knots match {
          case Nil =>
            Nil
          case (tx, ty) :: tail =>
            val (hx, hy) = before
            val distX = Math.abs(hx - tx)
            val distY = Math.abs(hy - ty)
            val (utx, uty) =
              if (distX > 1 || distY > 1) {
                if (distY == 0) (tx, ty) + (hx - tx, 0).unit
                else if (distX == 0) (tx, ty) + (0, hy - ty).unit
                else (tx, ty) + (hx - tx, hy - ty).unit
              } else
                (tx, ty)
            (utx, uty) :: updateKnots(tail, (utx, uty))
        }

      lines.foldLeft((start, start, List.fill(nKnots)((start, start)))) {
        case ((hx, hy, knots), move) =>
          val (dx, dy) = moveDir(move)
          val (uhx, uhy) = (hx + dx, hy + dy)
          val updatedKnots = updateKnots(knots, (uhx, uhy))
          map(updatedKnots.last._1)(updatedKnots.last._2) = true
          (uhx, uhy, updatedKnots)
      }

      map.map(_.count(identity)).sum
    }

    println("First Part: " + simulate(1))
    println("Second Part: " + simulate(9))
  }

}
