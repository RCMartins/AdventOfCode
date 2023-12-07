package year2023

import better.files.File

import scala.annotation.tailrec

object Problem06 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem06.in"))
    val Seq(timeLine, distLine) = input.lines.toSeq
    val times: Seq[Int] =
      timeLine.stripPrefix("Time:").split(" ").filter(_.nonEmpty).map(_.toInt).toSeq
    val dist: Seq[Int] =
      distLine.stripPrefix("Distance:").split(" ").filter(_.nonEmpty).map(_.toInt).toSeq

    val solutions: Seq[Int] =
      times.zip(dist).map { case (time, dist) =>
        (1 until time).count { timeHoldDown =>
          (time - timeHoldDown) * timeHoldDown >= dist
        }
      }

    println(
      solutions.product
    )
  }

}
