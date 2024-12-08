package code.year2024

import code.Utils._
import better.files.File

import scala.annotation.tailrec

object Problem06 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem06.in"))
    val inputLines: List[String] = input.lines.toList

    // true -> filled, false -> empty
    val mapGridisWall: Array[Array[Boolean]] =
      inputLines.map(_.map(_ == '#').toArray).toArray

    val (initialX, initialY) =
      inputLines.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.find(_._1 == '^').map(_._2).map((_, y))
      }.head

    val maxX = mapGridisWall.head.size
    val maxY = mapGridisWall.size

    val DIRS: Array[(Int, Int)] = Array((0, -1), (1, 0), (0, 1), (-1, 0))

    val seen: Array[Array[Array[Boolean]]] = Array.fill(maxY, maxX, 4)(false)

    def loop(): Boolean = {
      for {
        y <- 0 until maxY
        x <- 0 until maxX
        dir <- 0 until 4
      } seen(y)(x)(dir) = false

      @tailrec
      def loop(ix: Int, iy: Int, dir: Int): Boolean =
        if (!seen(iy)(ix)(dir)) {
          seen(iy)(ix)(dir) = true
          val (nx, ny) = (ix, iy) + DIRS(dir)
          if (nx >= 0 && nx < maxX && ny >= 0 && ny < maxY) {
            if (mapGridisWall(ny)(nx))
              loop(ix, iy, (dir + 1) % 4)
            else
              loop(nx, ny, dir)
          } else
            false
        } else {
          true
        }

      loop(initialX, initialY, 0)
    }

    loop()
    val nSeen: Int = seen.map(_.count(_.exists(identity))).sum

    println("First Part: " + nSeen)

    val resultPart2: Seq[(Int, Int)] =
      for {
        y <- 0 until maxY
        x <- 0 until maxX
        if !mapGridisWall(y)(x)
        if x != initialX || y != initialY
        if {
          mapGridisWall(y)(x) = true
          val res = loop()
          mapGridisWall(y)(x) = false
          res
        }
      } yield (x, y)

    println("Second Part: " + resultPart2.size)
  }

}
