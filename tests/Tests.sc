import $file.^.common.DataReader
import $file.^.algorithms.BFv2
import $file.^.algorithms.BFParallel

import $file.^.algorithms.Models, Models._

import $ivy.`org.scalatest::scalatest:3.2.10`, org.scalatest._
import org.scalatest.matchers.should._
import java.io.File
import org.scalactic.TolerantNumerics
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

new Tests().execute()

class Tests extends flatspec.AnyFlatSpec with Matchers {

  "Data reader" should "parse parse correctly" in {
    val result = DataReader.read(new File("tests/data/test.json"))
    result.isRight should be(true)
    result.toOption.get should contain.allElementsOf(expectedMap)
  }

  "BFv2" should "find all best arbitrage possibilities" in {
    val result = DataReader.read(new File("tests/data/test.json")).toOption.get
    val graph = Graph.fromApiMap(result)
    val arbitrage = new BFv2.BF(graph).arbitrage

    arbitrage.possibilities should not be empty
    arbitrage.possibilities should contain.allElementsOf(
      Array(
        ArbitragePossibility(
          "BTC",
          false,
          Vector(PathNode("JPY", "USD", 0.0098), PathNode("USD", "EUR", 0.7779), PathNode("EUR", "BTC", 0.01125), PathNode("BTC", "JPY", 12325.44))
        ),
        ArbitragePossibility("BTC", false, Vector(PathNode("USD", "EUR", 0.7779), PathNode("EUR", "BTC", 0.01125), PathNode("BTC", "USD", 115.65))),
        ArbitragePossibility("BTC", false, Vector(PathNode("USD", "JPY", 102.459), PathNode("JPY", "USD", 0.0098)))
      )
    )
  }

  "BFv2" should "tell if there is possibility from source" in {
    val EPS = 1e-4
    val result = DataReader.read(new File("tests/data/test.json")).toOption.get
    val graph = Graph.fromApiMap(result)
    val arbitrage = new BFv2.BF(graph).arbitrage("USD")

    arbitrage.possibilities should not be empty
    arbitrage.fromSourcePossibility.isDefined should be(true)
    arbitrage.fromSourcePossibility.get should be(
      ArbitragePossibility("USD", true, Vector(PathNode("USD", "EUR", 0.7779), PathNode("EUR", "BTC", 0.01125), PathNode("BTC", "USD", 115.65)))
    )
    arbitrage.fromSourcePossibility.get.income should be(1.012 +- EPS)
  }

  "BFv2" should "tell if there is possibility from source 2" in {
    val EPS = 1e-4
    val result = DataReader.read(new File("tests/data/test2.json")).toOption.get
    val graph = Graph.fromApiMap(result)
    val arbitrage = new BFv2.BF(graph).arbitrage("USD")
    val parallel = new BFParallel.BF(graph).arbitrage("USD")
    val parallelReady = Await.result(parallel, Duration.Inf)
    println(parallelReady.possibilities)
    arbitrage.possibilities should not be empty
    arbitrage.fromSourcePossibility.isDefined should be(true)
    arbitrage.fromSourcePossibility.get should be(
      ArbitragePossibility("USD", true, Vector(PathNode("USD", "BTC", 0.0088882), PathNode("BTC", "USD", 134.9448442)))
    )
    arbitrage.fromSourcePossibility.get.income should be(1.1994 +- EPS)
    parallelReady.fromSourcePossibility should be(arbitrage.fromSourcePossibility)
  }

  "BFv2" should "tell if there is possibility from source 3" in {
    val EPS = 1e-4
    val result = DataReader.read(new File("tests/data/test3.json")).toOption.get
    val graph = Graph.fromApiMap(result)
    val arbitrage = new BFv2.BF(graph).arbitrage("USD")

    arbitrage.possibilities should not be empty
    arbitrage.fromSourcePossibility.isDefined should be(true)
    arbitrage.fromSourcePossibility.get should be(
      ArbitragePossibility("USD", true, Vector(PathNode("USD", "BTC", 0.0088654), PathNode("BTC", "USD", 135.1351146)))
    )
    arbitrage.fromSourcePossibility.get.income should be(1.1981 +- EPS)
  }

  val expectedMap = {
    Map(
      "USD_EUR" -> 0.7779,
      "EUR_EUR" -> 1.0,
      "JPY_JPY" -> 1.0,
      "USD_BTC" -> 0.0083,
      "JPY_BTC" -> 8.11e-5,
      "BTC_EUR" -> 88.8499,
      "JPY_EUR" -> 0.0075,
      "USD_USD" -> 1.0,
      "BTC_BTC" -> 1.0,
      "USD_JPY" -> 102.459,
      "BTC_JPY" -> 12325.44,
      "EUR_JPY" -> 131.711,
      "EUR_USD" -> 1.2851,
      "EUR_BTC" -> 0.01125,
      "JPY_USD" -> 0.0098,
      "BTC_USD" -> 115.65
    )
  }
}
