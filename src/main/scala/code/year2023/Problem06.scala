package code.year2023

import better.files.File

import scala.annotation.tailrec

object Problem06 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem06.in"))
    val Seq(timeLine, distLine) = input.lines.toSeq
    val timesSeq: Seq[Int] =
      timeLine.stripPrefix("Time:").split(" ").filter(_.nonEmpty).map(_.toInt).toSeq
    val distSeq: Seq[Int] =
      distLine.stripPrefix("Distance:").split(" ").filter(_.nonEmpty).map(_.toInt).toSeq

    val solutions: Seq[Int] =
      timesSeq.zip(distSeq).map { case (time, dist) =>
        (1 until time).count { timeHoldDown =>
          (time - timeHoldDown) * timeHoldDown >= dist
        }
      }

    println(
      "First Part: " + solutions.product
    )

    val solutions2 = {
      val time = timesSeq.map(_.toString).mkString.toLong
      val dist = distSeq.map(_.toString).mkString.toLong
      (1L until time).count { timeHoldDown =>
        (time - timeHoldDown) * timeHoldDown >= dist
      }
    }

    println(
      "Second Part: " + solutions2
    )
  }

}
