package year2023

import better.files.File

import scala.collection.mutable

object Problem10 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem10.in"))
    val allLines = input.lines.toSeq

    val standardMapOriginal: IndexedSeq[IndexedSeq[Char]] =
      allLines.map(_.toIndexedSeq).toIndexedSeq

    val sizeX = standardMapOriginal.head.size
    val sizeY = standardMapOriginal.size

    val mainLoop: mutable.Set[(Int, Int)] = mutable.Set.empty

    {
      val standardMap: IndexedSeq[IndexedSeq[Char]] =
        standardMapOriginal.transpose

      val startPos: (Int, Int) =
        standardMap.zipWithIndex.flatMap { case (col, x) =>
          col.zipWithIndex.collectFirst { case ('S', y) => (x, y) }
        }.head

      var bx = -1
      var by = -1
      var counter = 0
      val queueNext: mutable.Queue[(Int, Int)] = mutable.Queue.empty

      queueNext.enqueue(startPos)

      while (queueNext.nonEmpty) {
        val (x, y) = queueNext.dequeue()
        val cur = standardMap(x)(y)
        mainLoop.add((x, y))

        def ifValid(dx: Int, dy: Int): Option[Char] = {
          val x2 = x + dx
          val y2 = y + dy
          standardMap.lift(x2).flatMap(_.lift(y2)).filter(_ => x2 != bx || y2 != by)
        }

        def goNext(dx: Int, dy: Int): Unit = {
          queueNext.enqueue((x + dx, y + dy))
          counter += 1
          bx = x
          by = y
        }

        def checkMult(seq: (Int, Int)*): Unit =
          seq.headOption.foreach { case (dx, dy) =>
            ifValid(dx, dy).map(_ => goNext(dx, dy)).getOrElse {
              checkMult(seq.tail: _*)
            }
          }

        cur match {
          case 'S' if counter == 0 =>
            ifValid(1, 0)
              .collect { case '-' | 'J' | '7' => goNext(1, 0) }
              .orElse(ifValid(-1, 0).collect { case '-' | 'L' | 'F' => goNext(-1, 0) })
              .orElse(ifValid(0, 1).collect { case '|' | '7' | 'F' => goNext(0, 1) })
              .orElse(ifValid(0, -1).collect { case '|' | 'J' | 'L' => goNext(0, -1) })
              .get
          case 'S' => ()
          case '|' => checkMult((0, -1), (0, 1))
          case '-' => checkMult((-1, 0), (1, 0))
          case 'L' => checkMult((1, 0), (0, -1))
          case 'J' => checkMult((0, -1), (-1, 0))
          case '7' => checkMult((-1, 0), (0, 1))
          case 'F' => checkMult((1, 0), (0, 1))
        }
      }

      val result1 = counter / 2

      println("First Part: " + result1)
    }

    val expandedMap =
      standardMapOriginal
        .map { line =>
          ' ' +: {
            {
              line.zip(line.tail).flatMap {
                case (c @ ('S' | '-' | 'L' | 'F'), 'S' | '-' | '7' | 'J') => Seq(c, '-')
                case (c, _)                                               => Seq(c, ' ')
              } :+ line.last
            } :+ ' '
          }
        }
        .transpose
        .map { col =>
          ' ' +: {
            {
              col.zip(col.tail).flatMap {
                case (c @ ('S' | '|' | '7' | 'F'), 'S' | '|' | 'L' | 'J') => Seq(c, '|')
                case (c, _)                                               => Seq(c, ' ')
              }
            } :+ col.last
          } :+ ' '
        }

    val invalidMoves: Seq[(Int, Int, Seq[Char])] =
      Seq(
        (1, 0, Seq('|', 'L', 'F', 'S')),
        (-1, 0, Seq('|', 'J', '7', 'S')),
        (0, 1, Seq('-', '7', 'F', 'S')),
        (0, -1, Seq('-', 'L', 'J', 'S'))
      )

    val seen = mutable.Set.empty[(Int, Int)]
    val queue: mutable.Queue[(Int, Int)] = mutable.Queue.empty
    queue.enqueue((0, 0))
    while (queue.nonEmpty) {
      val coor @ (x, y) = queue.dequeue()
      if (!seen(coor)) {
        seen.add(coor)
        invalidMoves.foreach { case (dx, dy, invalid) =>
          val x2 = x + dx
          val y2 = y + dy
          expandedMap.lift(x2).flatMap(_.lift(y2)).foreach { c =>
            if (
              !invalid.contains(c) ||
                (x2 % 2 == 1 && y2 % 2 == 1 && !mainLoop(((x2 - 1) / 2, (y2 - 1) / 2)))
            )
              queue.enqueue((x2, y2))
          }
        }
      }
    }

//    println(
//      expandedMap.transpose.zipWithIndex
//        .map { case (lineStr, y) =>
//          lineStr.zipWithIndex.map { case (c, x) =>
//            if (seen((x, y))) {
//              'X'
//            } else {
//              c
//            }
//          }.mkString
//        }
//        .mkString("\n")
//    )
//    println()

    val result2 =
      (0 until sizeX).map { x =>
        (0 until sizeY).count { y =>
          !mainLoop((x, y)) && !seen((x * 2 + 1, y * 2 + 1))
        }
      }.sum

    println("Second Part: " + result2)
  }

}
