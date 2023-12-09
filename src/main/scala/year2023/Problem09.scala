package year2023

import better.files.File

object Problem09 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem09.in"))
    val allLines = input.lines.toSeq

    val historySeq: Seq[Seq[Seq[Int]]] =
      allLines.map { line =>
        val initialValues = line.split(" ").map(_.toInt).toSeq

        def getDiffs(seq: Seq[Int]): Seq[Int] =
          seq.sliding(2).map { case Seq(a, b) => b - a }.toSeq

        def loop(initial: Seq[Int]): Seq[Seq[Int]] =
          if (initial.forall(_ == 0))
            Seq(initial)
          else
            initial +: loop(getDiffs(initial))

        loop(initialValues)
      }

    val result1: Seq[Int] =
      historySeq.map { history =>
        val values = history.map(_.last).reverse
        values.sum
      }

    println("First Part: " + result1.sum)

    val result2: Seq[Int] =
      historySeq.map { history =>
        val values = history.map(_.head).reverse
        values.foldLeft(0) { case (a, b) => b - a }
      }

    println("Second Part: " + result2.sum)
  }

}
