import $file.DataReader
import $file.Graph
import $file.BF_1

val currencies = DataReader.readUnsafe("data/example.json")
val graph = Graph.fromApiMap(currencies)

new BF_1.BF(graph).arbitrage
