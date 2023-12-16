package code.year2023

import better.files.File

import scala.util.chaining.scalaUtilChainingOps

object Problem15 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem15.in"))
    val line = input.lines.toList.head

    def calc(before: Int, char: Char): Int =
      (before + char) * 17 % 256

    val result1 =
      line
        .split(",")
        .map { inst =>
          inst.foldLeft(0) { case (before, char) =>
            calc(before, char)
          }
        }
        .sum

    println("First Part: " + result1)

    val finalHashMap: IndexedSeq[List[(String, Int)]] =
      line
        .split(",")
        .foldLeft(IndexedSeq.fill(256)(List.empty[(String, Int)])) {
          case (hashmap, s"$str-") =>
            val index = str.foldLeft(0) { case (before, char) => calc(before, char) }
            val before = hashmap(index)
            hashmap.updated(index, before.filterNot(_._1 == str))
          case (hashmap, s"$str=$n") =>
            val index = str.foldLeft(0) { case (before, char) => calc(before, char) }
            val before = hashmap(index)
            hashmap.updated(
              index,
              if (before.exists(_._1 == str))
                before.map {
                  case (label, _) if label == str => str -> n.toInt
                  case other                      => other
                }
              else
                before :+ (str, n.toInt)
            )
        }

    val result2 =
      finalHashMap.zipWithIndex
        .filter(_._1.nonEmpty)
        .map { case (list, box) =>
          list.zipWithIndex.foldLeft(0) { case (acc, ((_, n), index)) =>
            acc + (box + 1) * n * (index + 1)
          }
        }
        .sum

    println("Second Part: " + result2)
  }

}
