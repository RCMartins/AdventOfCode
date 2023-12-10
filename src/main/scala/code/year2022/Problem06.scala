package code.year2022

import better.files.File

object Problem06 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem06.in"))
    val lines = input.lines.toSeq

    def getUniqueWindow(size: Int): Int =
      lines.head
        .sliding(size)
        .zipWithIndex
        .find { case (window, _) =>
          window.distinct.length == size
        }
        .get
        ._2 + size

    println("First Part: " + getUniqueWindow(4))

    println("Second Part: " + getUniqueWindow(14))
  }

}
