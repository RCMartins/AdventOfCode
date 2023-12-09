package year2022

import better.files.File

object Problem03 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem03.in"))
    val lines = input.lines.toSeq

    def charToPoints(char: Char): Int =
      if (char.isLower) char - 'a' + 1 else char - 'A' + 27

    val result1 =
      lines.map { line =>
        val part1 = line.take(line.length / 2).toSet
        val part2 = line.drop(line.length / 2).toSet
        charToPoints(part1.intersect(part2).head)
      }

    println("First Part: " + result1.sum)

    val result2 =
      lines.grouped(3).map { case Seq(g1, g2, g3) =>
        charToPoints(g1.intersect(g2).intersect(g3).head)
      }

    println("Second Part: " + result2.sum)
  }

}
