import scala.collection._
import ammonite.runtime.tools.Grepper
import scala.annotation.tailrec
case class PathNode(from: String, to: String, cost: Double)

case class ArbitragePossibility(source: String, nodes: Vector[PathNode]) {
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

class BF(graph: Graph) {

  type Distances = mutable.Map[String, Double]
  type Predecessor = mutable.Map[String, String]
  private val initialValue = Double.MaxValue

  def arbitrage: Array[ArbitragePossibility] = {
    @tailrec
    def run(sources: Array[String]): Array[ArbitragePossibility] = {
      if (sources.isEmpty) {
        Array()
      } else {
        val cycles = arbitrage(sources.head)
        if (!cycles.isEmpty) {
          cycles
        } else {
          run(sources.tail)
        }
      }
    }
    run(graph.vertexes)
  }

  def arbitrage(source: String): Array[ArbitragePossibility] = {
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
      .map(cyc => ArbitragePossibility(source, cyc))
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
