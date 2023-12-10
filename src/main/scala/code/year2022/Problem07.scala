package code.year2022

import better.files.File

import scala.annotation.tailrec
import scala.collection.mutable

object Problem07 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2022/problem07.in"))
    val lines = input.lines.toList

    case class Node(name: String, size: Int, childred: Seq[String])

    val disk = mutable.Map.empty[List[String], Node]
    disk += List("/") -> Node("/", 0, Seq.empty)

    @tailrec
    def readCommand(commands: List[String], currentDir: List[String]): Unit =
      commands match {
        case Nil =>
          ()
        case command :: next =>
          command match {
            case "$ cd /" =>
              readCommand(next, List("/"))
            case "$ cd .." =>
              readCommand(next, currentDir.tail)
            case s"$$ cd $name" =>
              readCommand(next, name :: currentDir)
            case "$ ls" =>
              val (lsLines, otherLines) = next.span(!_.startsWith("$"))

              val nodes: Seq[Node] =
                lsLines.map {
                  case s"dir $name"   => Node(name, 0, Seq.empty)
                  case s"$size $name" => Node(name, size.toInt, Seq.empty)
                }
              nodes.foreach { node =>
                disk += (node.name :: currentDir) -> node
              }
              disk += currentDir -> Node(currentDir.head, 0, nodes.map(_.name))
              readCommand(otherLines, currentDir)
          }
      }

    readCommand(lines, List("/"))

    def sizeOfFolder(path: List[String]): Int = {
      val node = disk(path)
      node.size + node.childred.map(child => sizeOfFolder(child :: path)).sum
    }

    val result1 =
      disk.toSeq
        .filter { case (_, node) => node.size == 0 }
        .map { case (path, _) => sizeOfFolder(path) }
        .filter(_ <= 100000)
        .sum

    println("First Part: " + result1)

    val sizeRoot = sizeOfFolder(List("/"))
    val limit = 70000000 - 30000000
    val spaceRequired = sizeRoot - limit

    val result2 =
      disk.toSeq
        .filter { case (_, node) => node.size == 0 }
        .map { case (path, _) => sizeOfFolder(path) }
        .sorted
        .find(_ >= spaceRequired)
        .get

    println("Second Part: " + result2)
  }

}
