package code.year2024

import better.files.File

object Problem01 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem01.in"))
    val numbers: List[(String, String)] =
      input.lines.toList.flatMap(str =>
        str.split("\\s+").toList match {
          case List(a, b) => Some((a, b))
          case _          => None
        }
      )
    val (numbersA, numbersB) = numbers.unzip
    val sorted = numbersA.sorted.zip(numbersB.sorted)
    val sum = sorted.map { case (a, b) => Math.abs(a.toInt - b.toInt) }.sum

    println("First Part: " + sum)

    val count: Map[String, Int] =
      numbersB.groupBy(identity).map { case (key, value) => key -> value.size }

    val sumPartB = numbersA.map(a => a.toInt * count.getOrElse(a, 0)).sum

    println("Second Part: " + sumPartB)
  }

}
