package code.year2022

import better.files.File
import code.TupleExtensions

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Problem08 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem08.in"))
    val lines = input.lines.toIndexedSeq

    val valuesArray: Array[Array[Int]] = Array.ofDim(lines.head.length, lines.length)

    lines.zipWithIndex.foreach { case (line, y) =>
      line.zipWithIndex.foreach { case (value, x) =>
        valuesArray(x)(y) = value - '0'
      }
    }

    val visibleArray: Array[Array[Boolean]] = Array.fill(lines.head.length, lines.length)(false)

    val len = lines.length
    val dirsSeq: Seq[(Int, Int)] = Seq((1, 0), (0, 1), (-1, 0), (0, -1))
    val startPosSeq = Seq((0, 0), (0, 0), (len - 1, len - 1), (len - 1, len - 1))

    dirsSeq.zip(startPosSeq).foreach { case (dir @ (dx, dy), startPos) =>
      val outerDX = dir.swap
      val endOuterPos = startPos + outerDX * (len - 1)

      @tailrec
      def outerLoop(pos: (Int, Int)): Unit =
        if (pos != endOuterPos) {
          val endPos = pos + dir * (len - 1)

          @tailrec
          def innerLoop(x: Int, y: Int, height: Int): Unit = {
            val h = valuesArray(x)(y)
            if (h > height)
              visibleArray(x)(y) = true
            val newPos = (x + dx, y + dy)
            if (newPos != endPos)
              innerLoop(newPos._1, newPos._2, Math.max(h, height))
          }

          innerLoop(pos._1, pos._2, -1)
          outerLoop(pos + outerDX)
        }

      outerLoop(startPos)
    }

    val result1 =
      visibleArray.map(_.count(identity)).sum + 2

    println("First Part: " + result1)

    def scenicScoreDir(x: Int, y: Int, dx: Int, dy: Int): Int = {
      val centerHeight = valuesArray(x)(y)
      LazyList
        .from(1)
        .map(index => valuesArray.lift(x + dx * index).flatMap(_.lift(y + dy * index)))
        .takeWhile(_.nonEmpty)
        .flatten
        .pipe { seq =>
          if (seq.exists(_ >= centerHeight))
            seq.takeWhile(_ < centerHeight).size + 1
          else
            seq.size
        }
    }

    def scenicScore(x: Int, y: Int): Int =
      dirsSeq.map { case (dx, dy) =>
        scenicScoreDir(x, y, dx, dy)
      }.product

    val result2: Int =
      (1 until len - 1).map { x =>
        (1 until len - 1).map { y =>
          scenicScore(x, y)
        }.max
      }.max

    println("Second Part: " + result2)
  }

}
