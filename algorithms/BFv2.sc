import scala.collection._
import ammonite.runtime.tools.Grepper
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import $file.Models, Models._

case class Node(weight: Double = Double.MaxValue, to: String = "")
class BF(graph: Models.Graph) {

  type Distances = Map[String, Node]

  def arbitrage: ArbitragePossibilities = {
    @tailrec
    def loop(sources: Array[String]): Vector[ArbitragePossibility] = {
      if (sources.isEmpty) {
        Vector.empty
      } else {
        val cycles = run(sources.head)
        if (!cycles.isEmpty) {
          cycles
        } else {
          loop(sources.tail)
        }
      }
    }
    ArbitragePossibilities.make(loop(graph.vertexes))
  }

  def arbitrage(source: String): ArbitragePossibilities = {
    ArbitragePossibilities.make(run(source), Some(source))
  }

  private def run(source: String): Vector[ArbitragePossibility] = {
    val distances: Distances = relax(initializeSingleSource(source))

    detectNegativeCycles(distances).map {
      case (from, to) =>
        collectPath(distances.view.mapValues(_.to).toMap, from, to).sliding(2)
          .map {
            case Array(from, to) =>
              PathNode(from, to, graph.edges(from)(to).rate)
          }
    }.filter(!_.isEmpty)
      .map(_.toVector)
      .map(cycle => ArbitragePossibility.make(source, cycle))
      .toVector
  }

  def initializeSingleSource(source: String) = {
    (graph.vertexes.map(_ -> Node()).toMap[String, Node] + (source -> Node(0)))
  }

  def relax(initMap: Distances) = {
    @tailrec
    def loop(vertexes: Array[String], acc: Map[String, Node]): Map[String, Node] = {
      if (vertexes.isEmpty) {
        acc
      } else {
        val from = vertexes.head
        loop(
          vertexes.tail,
          acc ++ graph.edges(from).filter {
            case (to, edge) =>
              acc(to).weight > (acc(from).weight + edge.weight)
          }.map {
            case (to, edge) =>
              to -> Node(acc(from).weight + edge.weight, from)
          }
        )
      }
    }
    (for (_ <- 0 until graph.vertexes.size - 1) yield ()).foldLeft(initMap) {
      case (acc, _) =>
        loop(graph.vertexes, acc)
    }
  }

  private def detectNegativeCycles(distances: Distances): Array[(String, String)] = {
    graph.vertexes.flatMap { from =>
      graph.edges(from).filter {
        case (to, edge) =>
          distances(to).weight > (distances(from).weight + edge.weight)
      }.map {
        case (to, _) => (to -> from)
      }
    }
  }

  def collectPath(predecessor: Map[String, String], to: String, from: String): Array[String] = {
    @tailrec
    def loop(acc: Array[String], el: String): Array[String] = {
      if (acc.contains(el)) {
        acc
      } else (
        loop(acc :+ el, predecessor(el))
      )
    }
    (loop(Array(to, from), predecessor(from)) :+ to).reverse
  }
}
