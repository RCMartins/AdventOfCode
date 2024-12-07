package code.year2024

import better.files.File

object Problem07 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem07.in"))
    val inputLines: List[String] = input.lines.toList

    val equations: List[(Long, List[Long])] =
      inputLines.flatMap {
        _.split(": ").toList match {
          case List(expectedResult, numbersStr) =>
            Some((expectedResult.toLong, numbersStr.split(" ").map(_.toLong).toList))
          case _ =>
            None
        }
      }

    def isValidPart1(expectedResult: Long, initialNumbers: List[Long]): Boolean = {
      def loop(n: Long, numbers: List[Long]): Boolean = numbers match {
        case Nil =>
          n == expectedResult
        case head :: tail =>
          loop(n * head, tail) || loop(n + head, tail)
      }

      loop(initialNumbers.head, initialNumbers.tail)
    }

    val validPart1: List[(Long, List[Long])] =
      equations.filter { case (expectedResult, numbers) => isValidPart1(expectedResult, numbers) }

    println("First Part: " + validPart1.map(_._1).sum)

    def isValidPart2(expectedResult: Long, initialNumbers: List[Long]): Boolean = {
      def loop(n: Long, numbers: List[Long]): Boolean = numbers match {
        case Nil =>
          n == expectedResult
        case _ if n > expectedResult =>
          false
        case head :: tail =>
          loop((n.toString + head.toString).toLong, tail) ||
            loop(n * head, tail) ||
            loop(n + head, tail)
      }

      loop(initialNumbers.head, initialNumbers.tail)
    }

    val validPart2: List[(Long, List[Long])] =
      equations.filter { case (expectedResult, numbers) => isValidPart2(expectedResult, numbers) }

    println("Second Part: " + validPart2.map(_._1).sum)
  }

}
