package code.year2023

import better.files.File

import scala.util.chaining.scalaUtilChainingOps

object Problem13 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem13.in"))
    val lines = input.lines.toList.pipe { lines => if (lines.last.nonEmpty) lines :+ "" else lines }

    def loop(lines: List[String]): List[List[String]] = {
      val (puzzle, others) = lines.span(_.nonEmpty)
      if (puzzle.nonEmpty)
        puzzle :: loop(others.tail)
      else
        Nil
    }

    val puzzles: IndexedSeq[IndexedSeq[String]] =
      loop(lines).toIndexedSeq.map(_.toIndexedSeq)

    def originalMirrorSummary(puzzle: IndexedSeq[String]): Seq[Int] = {
      val puzzleTransposed = puzzle.transpose
      Seq(
        (1 until puzzle.size).flatMap { mirrorLine =>
          val beforeCount = mirrorLine
          val afterCount = puzzle.size - mirrorLine
          val minCount = Math.min(beforeCount, afterCount)

          val isAllSame =
            (1 to minCount).forall { dist =>
              puzzle(mirrorLine - dist) == puzzle(mirrorLine + dist - 1)
            }

          if (isAllSame) Some(mirrorLine * 100) else None
        },
        (1 until puzzleTransposed.size).flatMap { mirrorCol =>
          val beforeCount = mirrorCol
          val afterCount = puzzleTransposed.size - mirrorCol
          val minCount = Math.min(beforeCount, afterCount)

          val isAllSame =
            (1 to minCount).forall { dist =>
              puzzleTransposed(mirrorCol - dist) == puzzleTransposed(mirrorCol + dist - 1)
            }

          if (isAllSame) Some(mirrorCol) else None
        }
      ).flatten
    }

    val result1 = puzzles.flatMap(originalMirrorSummary).sum
    println("First Part: " + result1)

    val result2: Int =
      puzzles.flatMap { originalPuzzle =>
        val allSummaries =
          originalPuzzle.indices.flatMap { lineIndex =>
            val line = originalPuzzle(lineIndex)
            line.indices.flatMap { columnIndex =>
              originalMirrorSummary(
                originalPuzzle.updated(
                  lineIndex,
                  line.updated(columnIndex, if (line(columnIndex) == '.') '#' else '.'),
                )
              )
            }
          }.distinct

        val originalSummary = originalMirrorSummary(originalPuzzle).head
        allSummaries.filterNot { _ == originalSummary }
      }.sum
    println("Second Part: " + result2)
  }

}
