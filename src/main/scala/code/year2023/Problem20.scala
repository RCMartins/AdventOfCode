package code.year2023

import better.files.File

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Problem20 {

  def main(args: Array[String]): Unit = {
    val input = File(getClass.getResource("/year2023/problem20.in"))
    val lines = input.lines.toSeq

    sealed trait Module {
      def name: String
      def connections: Seq[String]
    }

    case class Broadcaster(name: String, connections: Seq[String]) extends Module

    case class FlipFlop(name: String, connections: Seq[String], isOn: Boolean) extends Module

    case class Conjuction(
        name: String,
        connections: Seq[String],
        previousSignals: Map[String, Boolean],
    ) extends Module

    val initialModules: Map[String, Module] =
      lines
        .map { line =>
          val (moduleName, moduleType, connectionsStr) =
            line match {
              case s"broadcaster -> $connections"  => ("broadcaster", "broadcaster", connections)
              case s"%$moduleName -> $connections" => (moduleName, "flip-flop", connections)
              case s"&$moduleName -> $connections" => (moduleName, "conjunction", connections)
            }

          val connections = connectionsStr.split(",").map(_.trim).toSeq
          if (moduleType == "broadcaster")
            moduleName -> Broadcaster(moduleName, connections)
          else if (moduleType == "flip-flop")
            moduleName -> FlipFlop(moduleName, connections, isOn = false)
          else
            moduleName -> Conjuction(moduleName, connections, Map.empty)
        }
        .toMap
        .pipe { allModules =>
          allModules.map {
            case (moduleName, Conjuction(_, connections, _)) =>
              val allInputs: Seq[String] =
                allModules.toSeq.filter(_._2.connections.contains(moduleName)).map(_._1)
              moduleName -> Conjuction(moduleName, connections, allInputs.map(_ -> false).toMap)
            case other =>
              other
          }
        }

    val initialModuleName = "broadcaster"
    val initialModuleConnections = initialModules(initialModuleName).connections

    def processOneCycle(
        cycleModules: Map[String, Module],
        cycleCount: Int
    ): (Map[String, Module], Int, Int) = {
      // false -> low, true -> high
      val queue = mutable.Queue[(String, String, Boolean)]()
      queue.enqueueAll(initialModuleConnections.map((initialModuleName, _, false)))
      val currentModules = mutable.Map[String, Module](cycleModules.toSeq: _*)
      var countLow = 0
      var countHigh = 0

      while (queue.nonEmpty) {
        val (previousModuleName, currentModuleName, signalIsHigh) = queue.dequeue()
        if (signalIsHigh)
          countHigh += 1
        else
          countLow += 1

        currentModules.get(currentModuleName) match {
          case None => // Nothing happens ?
            if (currentModuleName == "rx" && !signalIsHigh) {
              throw new Exception("Second part - Cycle where rx was low: " + cycleCount)
            }
          case Some(FlipFlop(_, _, _)) if signalIsHigh => // Nothing happens
          case Some(FlipFlop(name, connections, isOn)) =>
            currentModules.update(name, FlipFlop(name, connections, !isOn))
            queue.enqueueAll(connections.map((name, _, !isOn)))
          case Some(Conjuction(name, connections, inputs)) =>
            val updatedInputs = inputs.updated(previousModuleName, signalIsHigh)
            val nextPulseIsHigh = !updatedInputs.values.forall(identity)
            currentModules.update(name, Conjuction(name, connections, updatedInputs))
            queue.enqueueAll(connections.map((name, _, nextPulseIsHigh)))
          case _ =>
            ??? // :D
        }
      }
      (currentModules.toMap, countLow, countHigh)
    }

    val after1000Cycles: (Map[String, Module], Int, Int) =
      (1 to 1000).foldLeft((initialModules, 0, 0)) { case ((modules, countLow, countHigh), cycle) =>
        val (newModules, newCountLow, newCountHigh) = processOneCycle(modules, cycle)
        (newModules, countLow + newCountLow + 1, countHigh + newCountHigh)
      }

    val result1: Long = after1000Cycles._2.toLong * after1000Cycles._3.toLong

    println("First Part: " + result1)

    // TODO: Second part

    LazyList.from(1).foldLeft((initialModules, 0, 0)) {
      case ((modules, countLow, countHigh), cycle) =>
        if (cycle % 100000 == 0)
          println(s"Cycle $cycle")
        val (newModules, newCountLow, newCountHigh) = processOneCycle(modules, cycle)

        (newModules, countLow + newCountLow + 1, countHigh + newCountHigh)
    }
  }

}
