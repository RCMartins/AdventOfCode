package year2023

import better.files.File

object Problem03 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem03.in"))
    val rawMapData: Seq[String] = input.lines.toSeq
    val lineLen = rawMapData.head.length
    var allNumbers = Seq.empty[(Int, Int, Int, Int)]

    rawMapData.zipWithIndex.foreach { case (line, y) =>
      var x = 0
      while (x < lineLen) {
        if (line(x).isDigit) {
          val numStr = line.drop(x).takeWhile(_.isDigit)
          allNumbers = allNumbers :+ (numStr.toInt, y, x, x + numStr.length - 1)
          x += numStr.length
        }
        x += 1
      }
    }

    val filtered: Seq[Int] =
      allNumbers
        .filterNot { case (_, numY, numX1, numX2) =>
          (numX1 - 1 to numX2 + 1).forall { x =>
            (numY - 1 to numY + 1).forall { y =>
              rawMapData.lift(y).flatMap(_.lift(x)).forall(char => char.isDigit || char == '.')
            }
          }
        }
        .map(_._1)

    println("First Part: " + filtered.sum)

    val gears: Seq[Int] =
      rawMapData.zipWithIndex.flatMap { case (line, gearY) =>
        line.zipWithIndex.flatMap {
          case ('*', gearX) =>
            val numbersSeq: Seq[Int] =
              allNumbers
                .filter { case (_, numY, numX1, numX2) =>
                  gearX >= numX1 - 1 && gearX <= numX2 + 1 &&
                    gearY >= numY - 1 && gearY <= numY + 1
                }
                .map(_._1)

            numbersSeq match {
              case Seq(a, b) => Some(a * b)
              case _         => None
            }
          case _ =>
            None
        }
      }

    println("Second Part: " + gears.sum)
  }

}
