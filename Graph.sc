case class Edge(from: String, to: String, value: Double) {
  def weight = -scala.math.log(value)
}
case class Vertex(key: String)
case class Graph(vertexes: Array[String], edges: Array[Array[Edge]])

def fromApiMap(source: Map[String, Double]) = {
  val edges = source.map {
    case (fromTo, value) =>
      val (from, to) = parseCurrenciesKey(fromTo).left.map(throw _).merge
      Edge(from, to, value)
  }.groupBy(_.from).map {
    case (vertex, e) =>
      vertex -> e.toArray
  }.toMap
  Graph(edges.keySet.toArray, edges.values.toArray)
}

private def parseCurrenciesKey(fromTo: String) = {
  val array = fromTo.split("_")
  if (array.length != 2) {
    Left(new Exception("Unparsable key"))
  } else {
    Right((array(0), array(1)))
  }
}
