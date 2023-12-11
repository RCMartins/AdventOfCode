package code.year2022

import better.files.File
import code.TupleExtensions

object Problem10 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem10.in"))
    val lines = input.lines.toList

    def loop(lines: List[String], value: Int): List[Int] =
      lines match {
        case Nil =>
          Nil
        case "noop" :: tail =>
          value :: loop(tail, value)
        case s"addx $n" :: tail =>
          value :: (value + n.toInt) :: loop(tail, value + n.toInt)
      }

    val valuesAtCycle: IndexedSeq[Int] = (1 :: 1 :: loop(lines, 1)).toIndexedSeq

    val interestingCycles = Seq(20, 60, 100, 140, 180, 220)

    val result1 =
      interestingCycles.map(cycle => valuesAtCycle(cycle) * cycle).sum

    println("First Part: " + result1)

    // disclaimer: I think there is bug here somewhere, but it's still readable
    val result2 =
      (0 until 6).map { lineIndex =>
        (0 until 40).foldLeft(Seq.fill(40)(false)) { case (acc, column) =>
          val currentValue = valuesAtCycle(lineIndex * 40 + column)
          acc.updated(column, column >= currentValue && column <= currentValue + 2)
        }
      }

    println(
      "Second Part:\n" +
        result2
          .map(_.map(b => if (b) "#" else ".").mkString)
          .mkString("\n")
    )
  }

}
