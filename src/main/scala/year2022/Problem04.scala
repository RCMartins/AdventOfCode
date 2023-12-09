package year2022

import better.files.File

object Problem04 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem04.in"))
    val lines = input.lines.toSeq

    val result1 =
      lines.count { case s"$start1Str-$end1Str,$start2Str-$end2Str" =>
        val start1 = start1Str.toInt
        val end1 = end1Str.toInt
        val start2 = start2Str.toInt
        val end2 = end2Str.toInt

        start1 >= start2 && end1 <= end2 ||
          start2 >= start1 && end2 <= end1
      }

    println("First Part: " + result1)

    val result2 =
      lines.count { case s"$start1Str-$end1Str,$start2Str-$end2Str" =>
        val start1 = start1Str.toInt
        val end1 = end1Str.toInt
        val start2 = start2Str.toInt
        val end2 = end2Str.toInt

        start1 >= start2 && end1 <= end2 ||
          start2 >= start1 && end2 <= end1 ||
          start1 >= start2 && start1 <= end2 ||
          start2 >= start1 && start2 <= end1
      }

    println("Second Part: " + result2)
  }

}
