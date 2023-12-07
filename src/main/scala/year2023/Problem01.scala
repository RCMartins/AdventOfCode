package year2023

import better.files.File

object Problem01 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem01.in"))
    println(
      input.lines
        .map(line =>
          line.find(_.isDigit).get.toString +
            line.findLast(_.isDigit).get.toString
        )
        .map(_.toInt)
        .sum
    )
  }

}
