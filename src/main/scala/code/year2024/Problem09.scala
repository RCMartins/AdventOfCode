package code.year2024

import better.files.File

object Problem09 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2024/problem09.in"))
    val rawInputLine: String = input.contentAsString

    val expandedInputLine: Array[Int] =
      rawInputLine.toList
        .grouped(2)
        .zipWithIndex
        .flatMap {
          case (List(a, b), index) => List.fill(a - '0')(index) ++ List.fill(b - '0')(-1)
          case (List(a), index)    => List.fill(a - '0')(index)
        }
        .toArray

    {
      var currentReadIndex = expandedInputLine.length - 1
      var currentWriteIndex = 0

      while (currentWriteIndex < currentReadIndex) {
        if (expandedInputLine(currentWriteIndex) != -1) {
          currentWriteIndex += 1
        } else if (expandedInputLine(currentReadIndex) == -1) {
          currentReadIndex -= 1
        } else {
          expandedInputLine(currentWriteIndex) = expandedInputLine(currentReadIndex)
          expandedInputLine(currentReadIndex) = -1
          currentReadIndex -= 1
          currentWriteIndex += 1
        }
      }
    }

    def checksum(array: Array[Int]): Long =
      array.zipWithIndex.map {
        case (-1, _)        => 0
        case (value, index) => value.toLong * index
      }.sum

    println("First Part: " + checksum(expandedInputLine))

    sealed trait FileSystem
    case class FileBlock(id: Int, size: Int) extends FileSystem
    case class FreeSpace(size: Int) extends FileSystem

    var filesPart2: Array[FileSystem] =
      rawInputLine.toList
        .grouped(2)
        .zipWithIndex
        .flatMap {
          case (List(a, b), index) => List(FileBlock(index, a - '0'), FreeSpace(b - '0'))
          case (List(a), index)    => List(FileBlock(index, a - '0'))
        }
        .toArray

    {
      var currentReadIndex = filesPart2.length - 1
      var currentReadId = filesPart2.map {
        case FileBlock(id, _) => id
        case _                => -1
      }.max
      var currentWriteIndex = 0

      while (currentReadIndex > 0) {
        (filesPart2(currentWriteIndex), filesPart2(currentReadIndex)) match {
          case _ if currentWriteIndex >= currentReadIndex =>
            currentWriteIndex = 0
            currentReadIndex -= 1
            currentReadId -= 1
          case (_, FreeSpace(_)) =>
            currentReadIndex -= 1
          case (_, FileBlock(id, _)) if id != currentReadId =>
            currentReadIndex -= 1
          case (FileBlock(_, _), _) =>
            currentWriteIndex += 1
          case (FreeSpace(sizeFree), file @ FileBlock(_, sizeFile)) if sizeFree == sizeFile =>
            filesPart2(currentWriteIndex) = file
            filesPart2(currentReadIndex) = FreeSpace(sizeFree)
            currentWriteIndex = 0
            currentReadIndex -= 1
            currentReadId -= 1
          case (FreeSpace(sizeFree), file @ FileBlock(_, sizeFile)) if sizeFree > sizeFile =>
            filesPart2(currentReadIndex) = FreeSpace(sizeFile)
            val (partA, partB) = filesPart2.splitAt(currentWriteIndex)
            filesPart2 = partA ++ Array(file, FreeSpace(sizeFree - sizeFile)) ++ partB.tail
            currentWriteIndex = 0
            currentReadId -= 1
          case (FreeSpace(_), FileBlock(_, _)) =>
            currentWriteIndex += 1
        }
      }
    }

    val expandedFilesPart2: Array[Int] =
      filesPart2.flatMap {
        case FileBlock(id, size) => List.fill(size)(id)
        case FreeSpace(size)     => List.fill(size)(-1)
      }

    println("Second Part: " + checksum(expandedFilesPart2))
  }

}
