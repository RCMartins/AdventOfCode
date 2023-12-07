package year2023

import better.files.File

import scala.util.chaining.scalaUtilChainingOps

object Problem02 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem02.in"))
    val ids: Seq[Int] =
      input.lines.zipWithIndex.toSeq.flatMap { case (line, index) =>
        val gameContent = line.dropWhile(_ != ':').drop(1).trim
        val setsStrSeq = gameContent.split(";").toSeq.map(_.trim)
        val setsValid: Seq[Boolean] =
          setsStrSeq.map {
            _.split(",").toSeq
              .map(_.trim.split(" ").toSeq)
              .map { case Seq(amountStr, color) => (amountStr.toInt, color) }
              .map {
                case (amount, "red")   => amount <= 12
                case (amount, "green") => amount <= 13
                case (amount, "blue")  => amount <= 14
              }
              .pipe(_.forall(identity))
          }
        val isValid: Boolean = setsValid.forall(identity)
        Some(index + 1).filter(_ => isValid)
      }
    println("First Part: " + ids.sum)

    val powerSeq: Seq[Int] =
      input.lines.toSeq.map { line =>
        val gameContent = line.dropWhile(_ != ':').drop(1).trim
        val setsStrSeq = gameContent.split(";").toSeq.map(_.trim)
        setsStrSeq
          .foldLeft((0, 0, 0)) { case ((a, b, c), set) =>
            set
              .split(",")
              .toSeq
              .map(_.trim.split(" ").toSeq)
              .map { case Seq(amountStr, color) => (amountStr.toInt, color) }
              .foldLeft((a, b, c)) {
                case ((a, b, c), (amount, "red"))   => (Math.max(a, amount), b, c)
                case ((a, b, c), (amount, "green")) => (a, Math.max(b, amount), c)
                case ((a, b, c), (amount, "blue"))  => (a, b, Math.max(c, amount))
              }
          }
          .pipe { case (a, b, c) => a * b * c }
      }
    println("Second Part: " + powerSeq.sum)
  }

}
