package code.year2022

import better.files.File

import scala.annotation.tailrec

object Problem05 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem05.in"))
    val lines = input.lines.toSeq

    val (crateLines, moveLines) = lines.span(_.nonEmpty)
    val initialCrateLines: Seq[String] = {
      val maxSize = crateLines.map(_.length).max + 1
      crateLines.init.map(_.padTo(maxSize, ' '))
    }

    val initialCrates: IndexedSeq[List[String]] =
      initialCrateLines
        .map {
          _.grouped(4)
            .map {
              case s"[$c] " => Some(c)
              case _        => None
            }
            .toSeq
        }
        .transpose
        .map(_.flatten)
        .toIndexedSeq
        .map(_.toList)

    @tailrec
    def loop1(
        crates: IndexedSeq[List[String]],
        amount: Int,
        start: Int,
        end: Int,
    ): IndexedSeq[List[String]] =
      if (amount == 0)
        crates
      else {
        val elemStart :: remainingStart = crates(start)
        val updatedEnd = elemStart :: crates(end)
        loop1(
          crates
            .updated(start, remainingStart)
            .updated(end, updatedEnd),
          amount - 1,
          start,
          end,
        )
      }

    val result1 =
      moveLines.tail.foldLeft(initialCrates) { case (crates, s"move $amount from $start to $end") =>
        loop1(crates, amount.toInt, start.toInt - 1, end.toInt - 1)
      }

    println("First Part: " + result1.map(_.head).mkString)

    val result2 =
      moveLines.tail.foldLeft(initialCrates) { case (crates, s"move $amount from $start to $end") =>
        val (elemsStart, remainingStart) = crates(start.toInt - 1).splitAt(amount.toInt)
        val updatedEnd = elemsStart ++ crates(end.toInt - 1)
        crates
          .updated(start.toInt - 1, remainingStart)
          .updated(end.toInt - 1, updatedEnd)
      }

    println("Second Part: " + result2.map(_.head).mkString)
  }

}
