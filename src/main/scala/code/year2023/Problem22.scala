package code.year2023

import better.files.File

import scala.util.chaining.scalaUtilChainingOps

object Problem22 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem22.in"))
    val lines = input.lines.toSeq

    case class Brick(x1: Int, y1: Int, z1: Int, x2: Int, y2: Int, z2: Int) {

      def inXY(x: Int, y: Int): Boolean =
        x1 <= x && x <= x2 && y1 <= y && y <= y2

      def moveZ(z: Int): Brick =
        copy(z1 = z1 + z, z2 = z2 + z)

      def allXYPositions: IndexedSeq[(Int, Int)] =
        (x1 to x2).flatMap { x =>
          (y1 to y2).map { y =>
            (x, y)
          }
        }
    }

    val initialPositions: Seq[Brick] =
      lines.map { case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
        Brick(x1.toInt, y1.toInt, z1.toInt, x2.toInt, y2.toInt, z2.toInt).pipe {
          case Brick(x1, y1, z1, x2, y2, z2) if z1 > z2 =>
            Brick(x2, y2, z2, x1, y1, z1)
          case other =>
            other
        }
      }

    val sortedBricks: Seq[Brick] = initialPositions.sortBy(_.z1)

    def calcFinalPosition(stationaryBricks: Seq[Brick], brick: Brick): Brick =
      if (stationaryBricks.isEmpty) {
        val distanceGround = brick.z1 - 1
        brick.moveZ(-distanceGround)
      } else {
        val maxZ: Int =
          brick.allXYPositions.flatMap { case (x, y) =>
            stationaryBricks.map { case statBrick @ Brick(_, _, _, _, _, z2) =>
              if (statBrick.inXY(x, y))
                z2 + 1
              else
                1
            }
          }.max

        val distanceToGround = maxZ - brick.z1
        brick.moveZ(distanceToGround)
      }

    val finalBricks: Seq[Brick] =
      sortedBricks.foldLeft(Seq.empty[Brick]) { case (stationaryBricks, brick) =>
        val finalPosition = calcFinalPosition(stationaryBricks, brick)
        stationaryBricks :+ finalPosition
      }

    val amountOfSupportBricks: Map[Brick, Int] =
      finalBricks.map { brick =>
        brick ->
          brick.allXYPositions
            .flatMap { case (x, y) =>
              finalBricks.filter { otherBrick =>
                brick != otherBrick &&
                  otherBrick.inXY(x, y) &&
                  otherBrick.z2 == brick.z1 - 1
              }
            }
            .distinct
            .size
      }.toMap

    val result1: Int =
      finalBricks.count { brick =>
        val bricksAbove: Seq[Brick] =
          brick.allXYPositions.flatMap { case (x, y) =>
            finalBricks.filter { otherBrick =>
              brick != otherBrick &&
                otherBrick.inXY(x, y) &&
                otherBrick.z1 == brick.z2 + 1
            }
          }.distinct

        bricksAbove.forall(amountOfSupportBricks(_) >= 2)
      }

    println("First Part: " + result1)

    // Note: This is a slow solution (3-4 minutes to run), but it works
    val result2: Int =
      finalBricks.map { brick =>
        val finalBricksWithoutThis = finalBricks.filter(_ != brick).sortBy(_.z1)
        val newPositions =
          finalBricksWithoutThis.foldLeft(Seq.empty[Brick]) { case (stationaryBricks, brick) =>
            val finalPosition = calcFinalPosition(stationaryBricks, brick)
            stationaryBricks :+ finalPosition
          }

        finalBricksWithoutThis.zip(newPositions).count { case (oldBrick, newBrick) =>
          oldBrick != newBrick
        }
      }.sum

    println("Second Part: " + result2)
  }

}
