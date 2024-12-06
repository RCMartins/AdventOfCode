package code.year2024

import better.files.File

object Problem05 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem05.in"))
    val inputLines: List[String] = input.lines.toList

    val rulesList: Seq[(Int, Int)] =
      inputLines.filter(_.contains("|")).flatMap {
        _.split("\\|").toList match {
          case List(a, b) => Some((a.toInt, b.toInt))
          case _          => None
        }
      }

    // key must be printed before all pages in the value
    val rules: Map[Int, Seq[Int]] =
      rulesList.groupMap(_._1)(_._2)

    val checkLists: List[List[Int]] =
      inputLines.filter(_.contains(",")).map(_.split(",").toList.map(_.toInt))

    def listIsValid(list: List[Int]): Boolean =
      list.tails.toList.forall {
        case Nil | List(_) =>
          true
        case currentPage :: tail =>
          tail.forall(afterPage =>
            rules.get(afterPage) match {
              case None      => true
              case Some(seq) => !seq.contains(currentPage)
            }
          )
      }

    val validListsMiddlePart1: List[Int] =
      checkLists
        .filter(listIsValid)
        .map(list => list(list.size / 2))

    println("First Part: " + validListsMiddlePart1.sum)

    def fixList(originalList: List[Int]): List[Int] =
      originalList match {
        case Nil | List(_) =>
          originalList
        case currentPage :: tail =>
          val pageIndexToMoveOpt: Option[Int] =
            tail.zipWithIndex
              .find { case (afterPage, _) =>
                rules.get(afterPage).exists(_.contains(currentPage))
              }
              .map(_._2)

          pageIndexToMoveOpt match {
            case None =>
              currentPage :: fixList(tail)
            case Some(pageIndexToMove) =>
              val page = tail(pageIndexToMove)
              fixList(page :: currentPage :: tail.filterNot(_ == page))
          }
      }

    val validListsMiddlePart2: List[Int] =
      checkLists
        .filterNot(listIsValid)
        .map(fixList)
        .map(list => list(list.size / 2))

    println("Second Part: " + validListsMiddlePart2.sum)
  }

}
