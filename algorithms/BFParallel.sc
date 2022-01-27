import $ivy.`com.typesafe.akka::akka-stream:2.6.18`, akka.stream.scaladsl._
import scala.collection._
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import $file.Models, Models._
import scala.concurrent.Future
import akka.Done
import scala.concurrent.ExecutionContext
import akka.NotUsed
import akka.stream.Materializer
import akka.actor.ActorSystem
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
case class Node(weight: Double = Double.MaxValue, to: String = "")

class BF(graph: Models.Graph) {
  implicit val system: ActorSystem = ActorSystem("QuickStart")
  type Distances = ConcurrentHashMap[String, Node]

  def arbitrage(source: String)(implicit ex: ExecutionContext): Future[ArbitragePossibilities] = {
    run(source).map { possibilities =>
      ArbitragePossibilities.make(possibilities, Some(source))
    }
  }

  private def run(source: String)(implicit ex: ExecutionContext): Future[Vector[ArbitragePossibility]] = {
    for {
      distances <- relax(initializeSingleSource(source))
      cycles <- detectNegativeCycles(distances)
      res <- Future(
        cycles.map {
          case (from, to) => collectPath(distances.asScala.view.mapValues(_.to).toMap, from, to).sliding(2)
              .map {
                case Array(from, to) =>
                  PathNode(from, to, graph.edges(from)(to).rate)
              }
        }.filter(!_.isEmpty)
          .map(_.toVector)
          .map(cycle => ArbitragePossibility.make(source, cycle))
          .toVector
      )
    } yield res
  }

  def initializeSingleSource(source: String) = {
    new ConcurrentHashMap[String, Node]((graph.vertexes.map(_ -> Node()).toMap[String, Node] + (source -> Node(0))).asJava)
  }

  def relax(initMap: Distances)(implicit ex: ExecutionContext) = {
    Source(0 until graph.vertexes.size - 1).mapAsync(1) { _ =>
      Source(graph.vertexes).runWith(edgeSink(initMap))
    }.runWith(Sink.ignore).map(_ => initMap)

  }

  def edgeSink(distances: Distances)(implicit ex: ExecutionContext): Sink[String, Future[Done]] = {
    Sink.foreachAsync[String](graph.edges.values.head.size - 1) { from =>
      Future(
        graph.edges(from).filter {
          case (to, edge) =>
            distances.get(to).weight > (distances.get(from).weight + edge.weight)
        }.map {
          case (to, edge) =>
            distances.replace(to, Node(distances.get(from).weight + edge.weight, from))
        }
      )
    }
  }

  private def detectNegativeCycles(distances: Distances)(implicit ex: ExecutionContext): Future[Array[(String, String)]] = {
    val sink = Sink.foldAsync[Array[(String, String)], String](Array.empty) {
      case (acc, from) =>
        Future(
          acc ++ graph.edges(from).filter {
            case (to, edge) =>
              distances.get(to).weight > (distances.get(from).weight + edge.weight)
          }.map {
            case (to, _) => (to -> from)
          }.toArray
        )
    }
    Source(graph.vertexes).runWith(sink)
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
