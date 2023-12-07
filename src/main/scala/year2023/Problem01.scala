package year2023

import better.files.File

import scala.util.chaining.scalaUtilChainingOps

object Problem01 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem01.in"))
    println(
      "First Part: " +
        input.lines
          .map(line =>
            line.find(_.isDigit).get.toString +
              line.findLast(_.isDigit).get.toString
          )
          .map(_.toInt)
          .sum
    )
    val allNumbers1: Map[String, Int] =
      "123456789".toSeq.zipWithIndex.map { case (c, i) => c.toString -> i }.toMap
    val allNumbers2: Map[String, Int] =
      "one two three four five six seven eight nine".split(" ").toSeq.zipWithIndex.toMap
    val allNumbers: Map[String, Int] =
      (allNumbers1 ++ allNumbers2).map { case (k, v) => k -> (v + 1) }
    println(
      "Second Part: " +
        input.lines.map { line =>
          val first: Int =
            allNumbers
              .map { case (numberStr, number) => line.indexOf(numberStr) -> number }
              .filter(_._1 != -1)
              .minBy(_._1)
              ._2

          val second: Int =
            allNumbers
              .map { case (numberStr, number) => line.lastIndexOf(numberStr) -> number }
              .filter(_._1 != -1)
              .maxBy(_._1)
              ._2

          (first.toString + second.toString).toInt
        }.sum
    )
  }

}
