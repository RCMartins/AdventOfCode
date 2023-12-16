package code.year2023

import better.files.File

import scala.collection.mutable

object Problem16 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem16.in"))
    val mirrorMap: IndexedSeq[IndexedSeq[Char]] = input.lines.toIndexedSeq.map(_.toIndexedSeq)
    val lineSize = mirrorMap.head.size

    val seen: Array[Array[Array[Boolean]]] = Array.fill(mirrorMap.size, lineSize, 4)(false)

    val dirs: Array[(Int, Int)] =
      Array((-1, 0), (1, 0), (0, -1), (0, 1))

    val queue = mutable.Queue[(Int, Int, Int, Int)]()
    queue.enqueue((-1, 0, 1, 0))

    def isInside(x: Int, y: Int): Boolean =
      y >= 0 && y < mirrorMap.size && x >= 0 && x < lineSize

    while (queue.nonEmpty) {
      val (x, y, dx, dy) = queue.dequeue()
      val (uX, uY) = (x + dx, y + dy)
      val dirIsVertical = dx == 0
      val dirIndex = dirs.indexOf((dx, dy))
      if (x >= 0)
        seen(y)(x)(dirIndex) = true

      println(((x, y, dx, dy), queue))

      if (isInside(uX, uY)) {
        if (!seen(uY)(uX)(dirIndex)) {
          val c = mirrorMap(uY)(uX)
          println((uX, uY, c))
          if (c == '.')
            queue.enqueue((uX, uY, dx, dy))
          else if (c == '/')
            queue.enqueue((uX, uY, -dy, -dx))
          else if (c == '\\')
            queue.enqueue((uX, uY, dy, dx))
          else if (c == '-')
            if (dirIsVertical)
              queue
                .enqueue((uX, uY, -1, 0))
                .enqueue((uX, uY, 1, 0))
            else
              queue.enqueue((uX, uY, dx, dy))
          else // if (c == '|')
          if (dirIsVertical)
            queue.enqueue((uX, uY, dx, dy))
          else
            queue
              .enqueue((uX, uY, 0, -1))
              .enqueue((uX, uY, 0, 1))
        }
      }
    }

    val str =
      seen.zipWithIndex
        .map { case (line, index) =>
          f"$index%3d ${line.map(list => if (list.exists(identity)) 'X' else '.').mkString}"
        }
        .mkString("\n")
    println(
      str
    )
    println(str.count(_ == 'X'))

    val result1 =
      seen
        .map(_.count(_.exists(identity)))
        .sum

    println("First Part: " + result1)
    // println("Second Part: " + result2)
  }

}
