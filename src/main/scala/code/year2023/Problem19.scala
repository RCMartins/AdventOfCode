package code.year2023

import better.files.File

import scala.annotation.tailrec

object Problem19 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem19.in"))
    val lines = input.lines.toSeq

    val WorkFlowRegex = "([a-z]+)\\{(.*)}".r

    sealed trait Operator

    object Operator {
      case object Default extends Operator
      case class GreaterThan(name: Char, value: Int) extends Operator {
        override def toString: String = s"$name>$value"
      }
      case class LessThan(name: Char, value: Int) extends Operator {
        override def toString: String = s"$name<$value"
      }
    }

    val workflows: Map[String, List[(Operator, String)]] =
      lines.collect { case WorkFlowRegex(name, content) =>
        name -> content.split(",").map(_.trim).toList.map {
          case s"$name>$value:$next" =>
            Operator.GreaterThan(name.head, value.toInt) -> next
          case s"$name<$value:$next" =>
            Operator.LessThan(name.head, value.toInt) -> next
          case next =>
            Operator.Default -> next
        }
      }.toMap

    case class Interval(x1: Int, x2: Int, m1: Int, m2: Int, a1: Int, a2: Int, s1: Int, s2: Int) {
      private def splitAux(
          min: Int,
          max: Int,
          value: Int,
          lowDiff: Int,
          highDiff: Int,
      ): (Option[(Int, Int)], Option[(Int, Int)]) =
        (
          Some((min, value + lowDiff)).filter { case (min, max) => min <= max },
          Some((value + highDiff, max)).filter { case (min, max) => min <= max },
        )

      private def update(name: Char, min: Int, max: Int): Interval =
        name match {
          case 'x' => copy(x1 = min, x2 = max)
          case 'm' => copy(m1 = min, m2 = max)
          case 'a' => copy(a1 = min, a2 = max)
          case 's' => copy(s1 = min, s2 = max)
        }

      def split(
          name: Char,
          value: Int,
          lowDiff: Int,
          highDiff: Int,
      ): (Option[Interval], Option[Interval]) = {
        val (lower, higher) =
          name match {
            case 'x' => splitAux(x1, x2, value, lowDiff, highDiff)
            case 'm' => splitAux(m1, m2, value, lowDiff, highDiff)
            case 'a' => splitAux(a1, a2, value, lowDiff, highDiff)
            case 's' => splitAux(s1, s2, value, lowDiff, highDiff)
          }
        (
          lower.map { case (min, max) => update(name, min, max) },
          higher.map { case (min, max) => update(name, min, max) },
        )
      }

    }

    case class Part(x: Int, m: Int, a: Int, s: Int) {
      val sum: Int = x + m + a + s
      def getPart(code: Char): Int =
        code match {
          case 'x' => x
          case 'm' => m
          case 'a' => a
          case 's' => s
        }
    }

    val parts: Seq[Part] =
      lines.collect { case s"{x=$x,m=$m,a=$a,s=$s}" =>
        Part(x.toInt, m.toInt, a.toInt, s.toInt)
      }

    val startWorkFlow = workflows("in")

    def processPart1(part: Part, workflow: List[(Operator, String)]): Int = {
      def endOrNext(nextName: String): Int =
        if (nextName == "A") part.sum
        else if (nextName == "R") 0
        else processPart1(part, workflows(nextName))

      workflow match {
        case Nil =>
          0 // impossible...
        case (Operator.Default, nextWorkflow) :: _ =>
          endOrNext(nextWorkflow)
        case (Operator.GreaterThan(name, value), nextWorkflow) :: otherWorkflows =>
          part.getPart(name) match {
            case v if v > value => endOrNext(nextWorkflow)
            case _              => processPart1(part, otherWorkflows)
          }
        case (Operator.LessThan(name, value), nextWorkflow) :: otherWorkflows =>
          part.getPart(name) match {
            case v if v < value => endOrNext(nextWorkflow)
            case _              => processPart1(part, otherWorkflows)
          }
      }
    }

    val result1: Int =
      parts.map(processPart1(_, startWorkFlow)).sum

    println("First Part: " + result1)

    def processPart2(interval: Interval, workflow: List[(Operator, String)]): Long = {
      def endOrNext(i: Interval, nextName: String): Long =
        if (nextName == "A")
          (i.x2 - i.x1 + 1L) * (i.m2 - i.m1 + 1L) * (i.a2 - i.a1 + 1L) * (i.s2 - i.s1 + 1L)
        else if (nextName == "R")
          0L
        else
          processPart2(i, workflows(nextName))

      workflow match {
        case Nil =>
          0 // impossible...
        case (Operator.Default, nextWorkflow) :: _ =>
          endOrNext(interval, nextWorkflow)
        case (Operator.GreaterThan(name, value), nextWorkflow) :: otherWorkflows =>
          val (lower, higher) = interval.split(name, value, lowDiff = 0, highDiff = 1)
          higher.map(endOrNext(_, nextWorkflow)).getOrElse(0L) +
            lower.map(processPart2(_, otherWorkflows)).getOrElse(0L)
        case (Operator.LessThan(name, value), nextWorkflow) :: otherWorkflows =>
          val (lower, higher) = interval.split(name, value, lowDiff = -1, highDiff = 0)
          lower.map(endOrNext(_, nextWorkflow)).getOrElse(0L) +
            higher.map(processPart2(_, otherWorkflows)).getOrElse(0L)
      }
    }

    val result2 =
      processPart2(Interval(1, 4000, 1, 4000, 1, 4000, 1, 4000), startWorkFlow)

    println("Second Part: " + result2)
  }

}
