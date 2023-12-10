package code.year2022

import better.files.File

import scala.util.chaining.scalaUtilChainingOps

object Problem01 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem01.in"))
    val lines = input.lines.toSeq :+ ""

    def getTopX(topX: Int): Int =
      lines
        .foldLeft((Seq.empty[Int], 0)) {
          case ((record, group), "") =>
            ((record :+ group).sortBy(-_).take(topX), 0)
          case ((record, group), line) =>
            (record, group + line.toInt)
        }
        ._1
        .sum

    println("First Part: " + getTopX(1))
    println("Second Part: " + getTopX(3))
  }

}
