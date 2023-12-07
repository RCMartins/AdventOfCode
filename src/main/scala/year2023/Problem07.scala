package year2023

import better.files.File

import scala.util.chaining.scalaUtilChainingOps

object Problem07 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem07.in"))
    val allLines = input.lines.toSeq
    val allBids: Seq[(String, Int)] =
      allLines.map(_.split(" ").toSeq.pipe { case Seq(cards, bid) => (cards, bid.toInt) })

    val cardsValue = "AKQJT98765432".reverse
    def value(cards: String): Int = {
      val indexes = cards.map(cardsValue.indexOf(_))
      val groups = indexes.groupBy(identity)
      groups.size match {
        case 1                                  => 7
        case 2 if groups.exists(_._2.size == 4) => 6
        case 2                                  => 5
        case 3 if groups.exists(_._2.size == 3) => 4
        case 3                                  => 3
        case 4                                  => 2
        case 5                                  => 1
      }
    }

    def tieCompare(cards1: String, cards2: String): Boolean =
      cards1
        .zip(cards2)
        .map { case (c1, c2) =>
          cardsValue.indexOf(c1).compareTo(cardsValue.indexOf(c2))
        }
        .find(_ != 0)
        .get < 0

    val sorted: Seq[(String, Int)] =
      allBids.sortWith { case ((cards1, _), (cards2, _)) =>
        val value1 = value(cards1)
        val value2 = value(cards2)
        if (value1 == value2)
          tieCompare(cards1, cards2)
        else
          value1 < value2
      }

    println(
      sorted.zipWithIndex.map { case ((_, bid), index) => bid * (index + 1) }.sum
    )
  }

}
