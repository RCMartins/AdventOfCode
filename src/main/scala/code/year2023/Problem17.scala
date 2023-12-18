package code.year2023

import better.files.File

import scala.collection.mutable

object Problem17 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem17.in"))
    val cityMap = input.lines.toIndexedSeq.map(_.toIndexedSeq.map(_ - '0'))

    val sizeY = cityMap.size
    val sizeX = cityMap.head.size

    def calcWithParams(maxConsecutive: Int, minConsecutive: Int): Int = {
      val queue = mutable.PriorityQueue.empty[(Int, Int, Int, Int, Int)](Ordering.by(-_._1))
      val seen = mutable.Set.empty[(Int, Int, Int, Int)]

      val dirs = Seq((1, 0), (0, 1), (-1, 0), (0, -1))
      queue.enqueue((0, 0, 0, 0, 0))

      var result1 = 0
      while (queue.nonEmpty) {
        val (heat, x, y, dir, amountDir) = queue.dequeue()
        if (!seen((x, y, dir, amountDir))) {
          seen.add((x, y, dir, amountDir))

          def ifInside(x: Int, y: Int): Option[(Int, Int)] =
            Some((x, y)).filter(_ => x >= 0 && x < sizeX && y >= 0 && y < sizeY)

          def addNew(x: Int, y: Int, dir: Int, amount: Int): Unit =
            if (!seen((x, y, dir, amount)))
              queue.enqueue((heat + cityMap(y)(x), x, y, dir, amount))

          if (x == sizeX - 1 && y == sizeY - 1 && amountDir >= minConsecutive) {
            result1 = heat
            queue.clear()
          } else {
            val (dx1, dy1) = dirs(dir)
            ifInside(x + dx1, y + dy1).filter(_ => amountDir < maxConsecutive).foreach {
              case (nx, ny) =>
                addNew(nx, ny, dir, amountDir + 1)
            }
            val dir2 = (dir + 1) % 4
            val (dx2, dy2) = dirs(dir2)
            ifInside(x + dx2, y + dy2).filter(_ => amountDir >= minConsecutive).foreach {
              case (nx, ny) =>
                addNew(nx, ny, dir2, 1)
            }
            val dir3 = (dir + 3) % 4
            val (dx3, dy3) = dirs(dir3)
            ifInside(x + dx3, y + dy3).filter(_ => amountDir >= minConsecutive).foreach {
              case (nx, ny) =>
                addNew(nx, ny, dir3, 1)
            }
          }
        }
      }

      result1
    }

    println("First Part: " + calcWithParams(3, 0))
    println("Second Part: " + calcWithParams(10, 4))
  }

}
