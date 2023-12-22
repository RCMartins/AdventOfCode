package code.year2023

import better.files.File

object Problem12 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem12.in"))
    val lines = input.lines.toSeq

//    def loop(line: List[Char], acc: Int, currentTarget: Option[Int], nextTargets: List[Int]): Int =
//      if (currentTarget.exists(_ < acc))
//        0
//      else
//        line match {
//          case Nil if currentTarget.forall(_ == acc) && nextTargets.isEmpty =>
//            1
//          case Nil =>
//            0
//          case head :: next =>
//            head match {
//              case '.' if currentTarget.exists(_ != acc) =>
//                0
//              case '.' =>
//                loop(next, 0, None, nextTargets)
//              case '#' if currentTarget.isEmpty =>
//                nextTargets match {
//                  case Nil =>
//                    0
//                  case headTarget :: nextTargets =>
//                    loop(next, acc + 1, Some(headTarget), nextTargets)
//                }
//              case '#' =>
//                loop(next, acc + 1, currentTarget, nextTargets)
//              case '?' =>
//                val try0 = loop('.' :: next, acc, currentTarget, nextTargets)
//                val try1 = loop('#' :: next, acc, currentTarget, nextTargets)
//                try0 + try1
//            }
//        }
//
//    val result1 =
//      lines.map { case s"$mask $numbersStr" =>
//        val numbers = numbersStr.split(",").map(_.toInt)
//        val v = loop(mask.toList, 0, None, numbers.toList)
//        println(s"$mask $numbersStr $v")
//        v
//      }.sum
//
//    println("First Part: " + result1)
//
//    println(
//      lines
//        .flatMap { case s"$mask $numbersStr" =>
//          val numbers = numbersStr.split(",").map(_.toInt)
//          numbers
//        }
//        .distinct
//        .sorted
//        .mkString(", ")
//    )

    // Factorial function
    def factorial(n: BigInt): BigInt =
      if (n == 0) 1 else n * factorial(n - 1)

    // Function to calculate the number of combinations
    def combinations(n: BigInt, k: BigInt): BigInt =
      factorial(n) / (factorial(k) * factorial(n - k))

    val MaxSize = 100
    val MaxAmount = 14
    // possibilities(size of # or ?)(amount of #) = amount of possibilities
    val possibilities: Array[Array[Int]] =
      (1 to MaxSize).map { size =>
        (1 to MaxAmount).map { amount =>
          if (amount > size)
            0
          else
            combinations(size, amount).toInt
        }.toArray
      }.toArray

    println(
      possibilities
        .transpose
        .take(6)
        .map(_.take(6).mkString(", "))
        .mkString("\n")
    )

    ???

//    val result2 =
//      lines.map { case s"$mask $numbersStr" =>
//        val numbers = numbersStr.split(",").map(_.toInt)
//        val v = loop(
//          List.fill(3)(mask.toList :+ '?').flatten.init,
//          0,
//          None,
//          List.fill(3)(numbers.toList).flatten,
//        )
//        println(s"$mask $numbersStr $v")
//        v
//      }.sum

//    println("Second Part: " + result2)
  }

}
