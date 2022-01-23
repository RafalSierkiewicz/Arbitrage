import scala.collection._
import ammonite.runtime.tools.Grepper
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

case class Node(weight: Double = Double.MaxValue, to: String = "")
class BF(graph: Graph) {

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

case class Graph(vertexes: Array[String], edges: Map[String, Map[String, Edge]])

case class Edge(rate: Double) {
  val weight = -scala.math.log(rate)
}

object Graph {
  def fromApiMap(source: Map[String, Double]) = {
    val edges = source.map {
      case (fromTo, value) =>
        val (from, to) = parseCurrenciesKey(fromTo).left.map(throw _).merge
        (from, to, value)
    }.groupBy(_._1).map {
      case (vertex, e) =>
        vertex -> e.map { case (_, to, value) => to -> Edge(value) }.toMap
    }.toMap

    Graph(edges.keySet.toArray, edges)
  }

  private def parseCurrenciesKey(fromTo: String) = {
    val array = fromTo.split("_")
    if (array.length != 2) {
      Left(new Exception("Unparsable key"))
    } else {
      Right((array(0), array(1)))
    }
  }
}

case class ArbitragePossibilities(
  source: Option[String],
  fromSourcePossibility: Option[ArbitragePossibility],
  possibilities: Vector[ArbitragePossibility]
)
object ArbitragePossibilities {
  def make(possibilities: Vector[ArbitragePossibility], source: Option[String] = None) = {
    ArbitragePossibilities(source, possibilities.find(_.isFromSource), possibilities)
  }
}

case class ArbitragePossibility(source: String, isFromSource: Boolean, nodes: Vector[PathNode]) {
  val income = nodes.map(_.cost).reduce(_ * _)
}

case class PathNode(from: String, to: String, cost: Double)

object ArbitragePossibility {
  def make(source: String, nodes: Vector[PathNode]) = {
    val isFromSource = !nodes.isEmpty && nodes.head.from == source && nodes.last.to == source
    ArbitragePossibility(source, isFromSource, nodes)
  }
}
