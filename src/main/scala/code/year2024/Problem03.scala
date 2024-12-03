package code.year2024

import better.files.File

object Problem03 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem03.in"))
    val initialInputList: List[Char] = input.contentAsString.trim.toList

    class Parser(part1: Boolean) {

      def getMults(inputList: List[Char]): List[Int] =
        getMults(inputList, enabled = true)

      def getMults(inputList: List[Char], enabled: Boolean): List[Int] =
        inputList match {
          case Nil =>
            Nil
          case 'm' :: 'u' :: 'l' :: '(' :: others if enabled =>
            getNumber1(others)
          case 'd' :: 'o' :: '(' :: ')' :: others if !part1 =>
            getMults(others, enabled = true)
          case 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: others if !part1 =>
            getMults(others, enabled = false)
          case _ :: others =>
            getMults(others, enabled = enabled)
        }

      def getNumber1(inputList: List[Char]): List[Int] =
        if (inputList.headOption.exists(_.isDigit)) {
          val (number1, others) = inputList.span(_.isDigit)
          if (number1.sizeIs <= 3)
            getComma(number1.mkString.toInt, others)
          else
            getMults(others)
        } else
          getMults(inputList)

      def getComma(number1: Int, inputList: List[Char]): List[Int] =
        if (inputList.headOption.contains(','))
          getNumber2(number1, inputList.drop(1))
        else
          getMults(inputList)

      def getNumber2(number1: Int, inputList: List[Char]): List[Int] =
        if (inputList.headOption.exists(_.isDigit)) {
          val (number2, others) = inputList.span(_.isDigit)
          if (number2.sizeIs <= 3)
            getClosingParens(number1, number2.mkString.toInt, others)
          else
            getMults(others)
        } else
          getMults(inputList)

      def getClosingParens(number1: Int, number2: Int, inputList: List[Char]): List[Int] =
        if (inputList.headOption.contains(')'))
          (number1 * number2) :: getMults(inputList.drop(1))
        else
          getMults(inputList)
    }

    val mults1: List[Int] =
      new Parser(part1 = true).getMults(initialInputList)

    println("First Part: " + mults1.sum)

    val mults2: List[Int] =
      new Parser(part1 = false).getMults(initialInputList)

    println("Second Part: " + mults2.sum)
  }

}
