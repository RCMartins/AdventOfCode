package code.year2023

import better.files.File

object Problem21 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem21.in"))
    val originalGardenMap = input.lines.toIndexedSeq.map(_.toIndexedSeq)

    val (startX, startY) =
      originalGardenMap.zipWithIndex.flatMap { case (row, y) =>
        row.zipWithIndex.collect { case ('S', x) =>
          (x, y)
        }
      }.head

    val maxX = originalGardenMap.head.length
    val maxY = originalGardenMap.length

    // true -> is a garden plot, false -> is a rock
    val gardenMap: IndexedSeq[IndexedSeq[Boolean]] =
      originalGardenMap.map {
        _.map {
          case '.' | 'S' => true
          case _         => false
        }
      }

    def loopOnce(currentPositions: Seq[(Int, Int)]): Seq[(Int, Int)] =
      currentPositions
        .flatMap { case (x, y) =>
          Seq(
            (x - 1, y),
            (x + 1, y),
            (x, y - 1),
            (x, y + 1)
          )
        }
        .filter { case (x, y) =>
          x >= 0 && x < maxX &&
            y >= 0 && y < maxY &&
            gardenMap(y)(x)
        }
        .distinct

    val newPosSeq =
      (1 to 64).foldLeft(Seq((startX, startY))) { case (currentPositions, _) =>
        loopOnce(currentPositions)
      }

    val result1 = newPosSeq.length

    println("First Part: " + result1)
  }

}
