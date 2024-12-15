package code.year2024

import better.files.File

import scala.annotation.tailrec

object Problem12 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem12.in"))
    val lines: Seq[String] = input.lines.toSeq

    val grid: IndexedSeq[IndexedSeq[Char]] =
      lines.map(_.toIndexedSeq).toIndexedSeq

    val maxX = grid.head.length
    val maxY = grid.length

    def isInside(x: Int, y: Int): Boolean = x >= 0 && x < maxX && y >= 0 && y < maxY

    val dirs: IndexedSeq[(Int, Int)] = IndexedSeq((0, 1), (1, 0), (0, -1), (-1, 0))

    def countCostPart1(): Int = {
      val seen: Array[Array[Boolean]] = Array.fill(maxY, maxX)(false)

      def loop(ix: Int, iy: Int): (Int, Int) = {
        val perim: Int =
          dirs.count { case (dx, dy) =>
            !isInside(ix + dx, iy + dy) || grid(iy + dy)(ix + dx) != grid(iy)(ix)
          }

        dirs
          .map { case (dx, dy) =>
            val nx = ix + dx
            val ny = iy + dy
            if (isInside(nx, ny) && !seen(ny)(nx) && grid(ny)(nx) == grid(iy)(ix)) {
              seen(ny)(nx) = true
              val (areaOther, perimOther) = loop(nx, ny)
              (areaOther, perimOther)
            } else
              (0, 0)
          }
          .foldLeft((1, perim)) { case ((a1, p1), (a2, p2)) => (a1 + a2, p1 + p2) }
      }

      {
        for {
          x <- 0 until maxX
          y <- 0 until maxY
          if !seen(y)(x)
        } yield {
          seen(y)(x) = true
          val (area, perim) = loop(x, y)
          area * perim
        }
      }.sum
    }

    println("First Part: " + countCostPart1())

    def countCostPart2(): Int = {
      val seen: Array[Array[Boolean]] = Array.fill(maxY, maxX)(false)

      // index 0 -> top fence, index 1 - left fence
      val dirsFences: IndexedSeq[(Int, Int, Int)] =
        IndexedSeq((0, 1, 0), (1, 0, 1), (0, 0, 0), (0, 0, 1))

      def loop(x: Int, y: Int): Int = {
        val fences: Array[Array[Array[Boolean]]] = Array.fill(maxY + 1, maxX + 1, 2)(false)

        def isInsideFence(x: Int, y: Int): Boolean =
          x >= 0 && x <= maxX && y >= 0 && y <= maxY

        def getPositions(ix: Int, iy: Int): List[(Int, Int)] =
          dirs.toList
            .flatMap { case (dx, dy) =>
              val (nx, ny) = (ix + dx, iy + dy)
              if (isInside(nx, ny) && !seen(ny)(nx) && grid(ny)(nx) == grid(iy)(ix)) {
                seen(ny)(nx) = true
                (nx, ny) :: getPositions(nx, ny)
              } else
                Nil
            }

        def setFences(posList: List[(Int, Int)]): Unit = {
          posList.foreach { case (ix, iy) =>
            dirs.zip(dirsFences).foreach { case ((dx, dy), (fdx, fdy, fdz)) =>
              val (nx, ny) = (ix + dx, iy + dy)
              if (!isInside(nx, ny) || grid(ny)(nx) != grid(iy)(ix))
                fences(iy + fdy)(ix + fdx)(fdz) = true
            }
          }
        }

        val areaPosList: List[(Int, Int)] = (x, y) :: getPositions(x, y)
        setFences(areaPosList)

        @tailrec
        def clearFences(ix: Int, iy: Int, dx: Int, dy: Int, z: Int): Unit =
          if (isInsideFence(ix, iy) && fences(iy)(ix)(z)) {
            val sameFence =
              !isInside(ix, iy) ||
                !isInside(ix - dx, iy - dy) ||
                grid(iy)(ix) == grid(iy - dy)(ix - dx) || {
                  if (z == 0)
                    !isInside(ix, iy - 1) ||
                      !isInside(ix - dx, iy - dy - 1) ||
                      grid(iy - 1)(ix) == grid(iy - dy - 1)(ix - dx)
                  else
                    !isInside(ix - 1, iy) ||
                      !isInside(ix - dx - 1, iy - dy) ||
                      grid(iy)(ix - 1) == grid(iy - dy)(ix - dx - 1)
                }

            if (sameFence) {
              fences(iy)(ix)(z) = false
              clearFences(ix + dx, iy + dy, dx, dy, z)
            }
          }

        for {
          x <- 0 until maxX
          y <- 0 to maxY
          if fences(y)(x)(0)
        } yield {
          clearFences(x + 1, y, 1, 0, 0)
        }
        for {
          x <- 0 to maxX
          y <- 0 until maxY
          if fences(y)(x)(1)
        } yield {
          clearFences(x, y + 1, 0, 1, 1)
        }

        val countFences = {
          for {
            x <- 0 to maxX
            y <- 0 to maxY
            z <- 0 to 1
            if fences(y)(x)(z)
          } yield 1
        }.size

        areaPosList.size * countFences
      }

      {
        for {
          y <- 0 until maxY
          x <- 0 until maxX
          if !seen(y)(x)
        } yield {
          seen(y)(x) = true
          loop(x, y)
        }
      }.sum
    }

    println("Second Part: " + countCostPart2())
  }

}
