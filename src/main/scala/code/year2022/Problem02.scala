package code.year2022

import better.files.File

object Problem02 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem02.in"))
    val lines = input.lines.toSeq

    val result1 =
      lines.map { case s"$opp $my" =>
        (opp, my) match {
          case ("A", "X") => 4
          case ("B", "X") => 1
          case ("C", "X") => 7
          case ("A", "Y") => 8
          case ("B", "Y") => 5
          case ("C", "Y") => 2
          case ("A", "Z") => 3
          case ("B", "Z") => 9
          case ("C", "Z") => 6
        }
      }

    println("First Part: " + result1.sum)

    val result2 =
      lines.map { case s"$opp $my" =>
        (opp, my) match {
          case ("A", "X") => 3
          case ("B", "X") => 1
          case ("C", "X") => 2
          case ("A", "Y") => 4
          case ("B", "Y") => 5
          case ("C", "Y") => 6
          case ("A", "Z") => 8
          case ("B", "Z") => 9
          case ("C", "Z") => 7
        }
      }

    println("Second Part: " + result2.sum)
  }

}
