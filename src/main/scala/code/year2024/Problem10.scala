package code.year2024

import better.files.File

object Problem10 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem10.in"))
    val lines: List[String] = input.lines.toList

    val grid: IndexedSeq[IndexedSeq[Int]] = lines.map(_.map(_ - '0').toIndexedSeq).toIndexedSeq

    val maxX = grid(0).length
    val maxY = grid.length

    val pairs: IndexedSeq[(Int, Int)] = {
      val dx: IndexedSeq[Int] = IndexedSeq(1, 0, -1, 0)
      val dy: IndexedSeq[Int] = IndexedSeq(0, 1, 0, -1)
      dx.zip(dy)
    }

    def countTailHeadsPart1(ix: Int, iy: Int): Int = {
      val seen: Array[Array[Boolean]] = Array.fill(maxY, maxX)(false)

      def countTailHeads(ix: Int, iy: Int, n: Int): Unit =
        pairs.foreach { case (dx, dy) =>
          val nx = ix + dx
          val ny = iy + dy
          if (nx >= 0 && nx < maxX && ny >= 0 && ny < maxY && !seen(ny)(nx)) {
            val gNum = grid(ny)(nx)
            if (gNum == n + 1) {
              seen(ny)(nx) = true
              countTailHeads(nx, ny, n + 1)
            }
          }
        }

      countTailHeads(ix, iy, 0)

      {
        for {
          y <- 0 until maxY
          x <- 0 until maxX
        } yield if (seen(y)(x) && grid(y)(x) == 9) 1 else 0
      }.sum
    }

    val trailHeadsPointsPart1: Seq[Int] =
      for {
        y <- 0 until maxY
        x <- 0 until maxX
        if grid(y)(x) == 0
      } yield countTailHeadsPart1(x, y)

    println("First Part: " + trailHeadsPointsPart1.sum)

    def countTailHeadsPart2(ix: Int, iy: Int, n: Int): Int =
      pairs.map { case (dx, dy) =>
        val nx = ix + dx
        val ny = iy + dy
        if (nx >= 0 && nx < maxX && ny >= 0 && ny < maxY) {
          val gNum = grid(ny)(nx)
          if (gNum == n + 1) {
            if (gNum == 9)
              1
            else
              countTailHeadsPart2(nx, ny, n + 1)
          } else
            0
        } else
          0
      }.sum

    val trailHeadsPointsPart2: Seq[Int] =
      for {
        y <- 0 until maxY
        x <- 0 until maxX
        if grid(y)(x) == 0
      } yield countTailHeadsPart2(x, y, 0)

    println("Second Part: " + trailHeadsPointsPart2.sum)
  }

}
