package code.year2024

import better.files.File

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Problem11 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem11.in"))
    val content: String = input.contentAsString

    val blinkCache: mutable.Map[BigInt, List[BigInt]] = mutable.Map.empty
    blinkCache.put(0, List(1))

    val initialNumbers: List[BigInt] = content.split(" ").map(_.toInt).map(BigInt(_)).toList

    def blink(n: BigInt): List[BigInt] =
      blinkCache.getOrElseUpdate(
        n, {
          val nStr = n.toString
          if (nStr.length % 2 == 0)
            nStr.splitAt(nStr.length / 2).pipe { case (a, b) => List(BigInt(a), BigInt(b)) }
          else
            List(n * 2024)
        }
      )

    def smartCalc(loops: Int, numbers: List[BigInt]): BigInt = {
      val listWithCount: Seq[(BigInt, BigInt)] =
        numbers.groupBy(identity).map { case (k, v) => (k, BigInt(v.size)) }.toList
      (1 to loops)
        .foldLeft(listWithCount) { (acc, _) =>
          acc
            .flatMap { case (n, count) => blink(n).map((_, count)) }
            .groupBy(_._1)
            .toList
            .map { case (k, v) => (k, v.map(_._2).sum) }
        }
        .map(_._2)
        .sum
    }

    println("First Part: " + smartCalc(25, initialNumbers))

    println("Second Part: " + smartCalc(75, initialNumbers))
  }

}
