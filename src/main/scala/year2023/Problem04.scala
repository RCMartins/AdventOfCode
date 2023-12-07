package year2023

import better.files.File

object Problem04 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem04.in"))
    val gamesLines: Seq[String] = input.lines.toSeq.map(_.split(":")(1).trim)
    val points: Seq[Int] =
      0 +: LazyList.continually(2).scanLeft(1) { case (a, b) => a * b }.take(10).toList

    val pointsSeq: Seq[Int] =
      gamesLines.map { gameStr =>
        val Seq(winningCardsStr, myCardsStr) = gameStr.split("\\|").toSeq.map(_.trim)
        val winningCards = winningCardsStr.split(" ").toSeq.filter(_.nonEmpty).map(_.toInt)
        val myCards = myCardsStr.split(" ").toSeq.filter(_.nonEmpty).map(_.toInt)
        val filteredCards = myCards.filter(winningCards.contains)
        points(filteredCards.size)
      }

    println(pointsSeq.sum)
  }

}
