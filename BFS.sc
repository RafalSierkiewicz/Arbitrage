import $file.DataReader

val currentExchange = DataReader.read("data/example.json").left.map(throw _).merge
val graph = new Graph(prepareGraph(currentExchange).to(collection.mutable.Map))

graph.prettyPrint

def prepareGraph(exchange: Map[String, Double]): Map[String, Vector[Node]] = {
  exchange.map {
    case (fromToCurr, value) =>
      (fromToCurr.split("_")(0), fromToCurr.split("_")(1), value)
  }.filter(el => el._1 != el._2)
    .groupBy(_._1)
    .map {
      case (currency, neighboorList) =>
        currency -> neighboorList.map { case (_, to, value) => Node(to, value) }.toVector
    }
}

object Label {
  sealed trait Label
  case object NonExplored extends Label
  case object Explored extends Label
}

case class Node(name: String, d: Double = 1.0, label: Label.Label = Label.NonExplored, prev: Option[Node] = None)
class Graph(graph: scala.collection.mutable.Map[String, Vector[Node]]) {
  // represented as neighboor list

  def prettyPrint = {
    graph.foreach {
      case (currency, neighboors) =>
        println(s"$currency" + s"\t ${neighboors.sortBy(_.name).map(node => s"${node.name}, ${node.d}, ${node.label}").mkString(" | ")}")
    }
  }
}
