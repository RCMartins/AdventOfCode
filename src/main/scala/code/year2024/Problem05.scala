package code.year2024

import better.files.File

import scala.annotation.tailrec

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

    val total: Int =
      checkLists
        .filter { list =>
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
        }
        .map { list =>
          list(list.size / 2)
        }
        .sum

    println("First Part: " + total)
  }

}
