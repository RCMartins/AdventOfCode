package code.year2023

import better.files.File

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Problem14 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem14.in"))
    val columns = input.lines.toList.transpose

    val result1: List[Int] =
      columns.map { column =>
        val colIndexed = column.zipWithIndex
        val walls = colIndexed.filter(_._1 == '#').map(_._2)
        val rocks = colIndexed.filter(_._1 == 'O').map(_._2)
        val size = column.length

        @tailrec
        def loop(
            currentIndex: Int,
            currentWalls: List[Int],
            currentRocks: List[Int],
            acc: List[Int]
        ): List[Int] =
          if (currentIndex == size)
            acc
          else {
            (currentWalls, currentRocks) match {
              case (_, Nil) =>
                acc
              case (Nil, _ :: _) =>
                currentRocks.indices.map(_ + currentIndex).toList ::: acc
              case (wall :: _, rock :: otherRocks) if wall > rock =>
                loop(currentIndex + 1, currentWalls, otherRocks, currentIndex :: acc)
              case (wall :: otherWalls, _ :: _) =>
                loop(wall + 1, otherWalls, currentRocks, acc)
            }
          }

        val indices =
          if (walls.isEmpty)
            rocks.indices.toList
          else
            loop(LazyList.from(0).find(i => !walls.contains(i)).get, walls, rocks, Nil)

        indices.map(size - _).sum
      }

    println("First Part: " + result1.sum)
  }

}
