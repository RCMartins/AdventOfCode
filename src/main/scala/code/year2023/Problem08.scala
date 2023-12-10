package code.year2023

import better.files.File

import scala.annotation.tailrec

object Problem08 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem08.in"))
    val allLines = input.lines.toSeq
    val moves: String = allLines.head

    val NetWorkRegex = "(.{3}) = \\((.{3}), (.{3})\\)".r

    val network: Map[String, (String, String)] =
      allLines
        .drop(2)
        .map { case NetWorkRegex(a, b, c) => a -> (b, c) }
        .toMap

    val Start = "AAA"
    val End = "ZZZ"

    @tailrec
    def loopUntilEnd(start: String, moveIndex: Int, count: Int, endF: String => Boolean): Int =
      if (endF(start))
        count
      else {
        val (left, right) = network(start)
        loopUntilEnd(
          if (moves(moveIndex) == 'L') left else right,
          (moveIndex + 1) % moves.length,
          count + 1,
          endF,
        )
      }

    println(
      "First Part: " + loopUntilEnd(Start, 0, 0, _ == End)
    )

    val secondStartNodes: Seq[String] = network.keys.filter(_.endsWith("A")).toSeq

    @tailrec
    def gcd(a: Long, b: Long): Long =
      if (b == 0) a else gcd(b, a % b)

    def lcm(a: Long, b: Long): Long =
      (a * b).abs / gcd(a, b)

    val repeatCycle: Seq[Long] =
      secondStartNodes.map(loopUntilEnd(_, 0, 0, _.endsWith("Z")))

    val lcmOfAll = repeatCycle.reduce((a, b) => lcm(a, b))

    println(
      "Second Part: " + lcmOfAll
    )
  }

}
