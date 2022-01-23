import scala.collection._
import ammonite.runtime.tools.Grepper
import scala.annotation.tailrec

class BF(graph: Graph) {

  type Distances = mutable.Map[String, Double]
  type Predecessor = mutable.Map[String, String]
  private val initialValue = Double.MaxValue

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
    val dist: Distances = initializeSingleSource(source)
    val predecessor = relax(dist)

    detectNegativeCycles(dist).map {
      case (from, to) =>
        collectPath(predecessor, from, to).sliding(2)
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
    val init = graph.vertexes.map(_ -> initialValue).to(mutable.Map)
    init.update(source, 0)
    init
  }

  def relax(distances: Distances) = {
    val pre: Predecessor = new mutable.TreeMap()

    for (_ <- 0 until graph.vertexes.size - 1) {
      graph.vertexes.foreach { from =>
        graph.edges(from).filter {
          case (to, edge) =>
            distances(to) > (distances(from) + edge.weight)
        }.foreach {
          case (to, edge) =>
            distances.update(to, distances(from) + edge.weight)
            pre.update(to, from)
        }
      }
    }
    pre
  }

  private def detectNegativeCycles(distances: Distances): Array[(String, String)] = {
    graph.vertexes.flatMap { from =>
      graph.edges(from).filter {
        case (to, edge) =>
          distances(to) > (distances(from) + edge.weight)
      }.map {
        case (to, _) => (to -> from)
      }
    }
  }

  def collectPath(predecessor: Predecessor, to: String, from: String): Array[String] = {
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
  def prettyPrint = {
    val space = "\t|\t"
    println(s"Possibile arbitrage")
    println(s"From${space}To${space}Cost")
    nodes
      .map(node => s"${node.from}${space}${node.to}${space}${node.cost}")
      .foreach(println)
    println(s"Income ${income}")
  }

  def scopedPrint = {
    println(nodes.map(_.from).mkString(" -> ") + s" -> ${nodes.last.to} profit ${income}")
  }
}

case class PathNode(from: String, to: String, cost: Double)

object ArbitragePossibility {
  def make(source: String, nodes: Vector[PathNode]) = {
    val isFromSource = !nodes.isEmpty && nodes.head.from == source && nodes.last.to == source
    ArbitragePossibility(source, isFromSource, nodes)
  }
}