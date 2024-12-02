package code.year2024

import better.files.File

import scala.annotation.tailrec

object Problem02 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem02.in"))
    val numbers: List[List[Int]] = input.lines.map(_.split("\\s+").map(_.toInt).toList).toList

    val DiffLimit = 3

    @tailrec
    def isSafePart1Loop(numbers: List[Int], incresingFlag: Boolean): Boolean = {
      numbers match {
        case _ :: Nil =>
          true
        case first :: second :: _
            if incresingFlag && (second <= first || (second - first) > DiffLimit) =>
          false
        case _ :: second :: others if incresingFlag =>
          isSafePart1Loop(second :: others, incresingFlag)
        case first :: second :: _
            if !incresingFlag && (second >= first || (first - second) > DiffLimit) =>
          false
        case _ :: second :: others if !incresingFlag =>
          isSafePart1Loop(second :: others, incresingFlag)
        case _ =>
          false
      }
    }

    def isSafePart1(numbers: List[Int]) = numbers match {
      case first :: second :: _ if first == second => false
      case report @ first :: second :: _           => isSafePart1Loop(report, first < second)
      case _                                       => false
    }

    val countSafePart1 = numbers.count(isSafePart1)
    println("First Part: " + countSafePart1)

    val countSafePart2 = numbers.count { report =>
      isSafePart1(report) ||
        report.indices.exists { index =>
          val reportEdited = report.take(index) ++ report.drop(index + 1)
          isSafePart1(reportEdited)
        }

    }
    println("Second Part: " + countSafePart2)
  }

}
