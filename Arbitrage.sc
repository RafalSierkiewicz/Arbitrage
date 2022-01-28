import $ivy.`org.http4s::http4s-blaze-client:0.21.5`, org.http4s.client.blaze._, org.http4s.client._
import $ivy.`org.slf4j:slf4j-nop:1.7.30`
import $file.common.DataReader
import $file.algorithms.Models, Models._
import $file.algorithms.BFv2, BFv2._
import $file.algorithms.BFParallel, BFParallel._

import $file.algorithms.BF_lib
import $file.apis.PriceApi, PriceApi._

import cats.effect._
import scala.concurrent.duration._
import org.slf4j.LoggerFactory
import scala.concurrent.ExecutionContext
import scala.concurrent.Await

implicit val EC = scala.concurrent.ExecutionContext.global
implicit val CS = IO.contextShift(EC)
val client = BlazeClientBuilder[IO](EC).resource.map { client =>
  IO.pure(new PriceApiClient(client, "https://fx.priceonomics.com/v1/rates/"))
}.allocated

@main
def arbitrage(source: String): Unit = {
  main(Some(source))
}

@main
def arbitrageAll(): Unit = {
  main(None)
}

def main(source: Option[String]): Unit = {
  val currencies = client.flatMap(_._1.flatMap(_.getPrices)).unsafeRunTimed(1.minute).get
  val bellmanFord = new BFv2.BF(Graph.fromApiMap(currencies))
  val possibilities = source match {
    case Some(currency) =>
      bellmanFord.arbitrage(currency.toUpperCase())
    case None =>
      bellmanFord.arbitrage
  }
  val parallel = new BFParallel.BF(Graph.fromApiMap(currencies)).arbitrage("USD")
  ArbitragePresenter.print(possibilities)

  println(s"\nParallel execution for USD \n")
  Await.ready(parallel.map(ArbitragePresenter.print), Duration.Inf)
}

client.flatMap(_._2).unsafeRunSync()

object ArbitragePresenter {
  def print(arbitrage: ArbitragePossibilities) = {
    val space = "=" * 4
    println(
      (Vector(s"${space} Arbitrage Possibilities ${space}") ++
        arbitrage.fromSourcePossibility
          .filter(_ => arbitrage.source.isDefined)
          .filter(_.income > 1.0).headOption.map { fromSource =>
            s"${space} Possibility from source found ${space}\n" +
              getPossibilityString(fromSource)
          }.orElse(arbitrage.source.map(sr => s"Arbitrage from source ${sr} not found")).toVector
        ++ Vector(s"${space} Showing all possibilities ${space}") ++
        arbitrage.possibilities
          .filter(_.income > 1.0)
          .sortBy(_.income)(Ordering[Double].reverse).map(getPossibilityString)).mkString("\n\n")
    )

  }

  private def getPossibilityString(possibility: ArbitragePossibility) = {
    val space = "\t|\t"
    (Vector(s"Possibile arbitrage") ++
      Vector(s"From${space}To${space}Cost") ++
      possibility.nodes
        .map(node => s"${node.from}${space}${node.to}${space}${node.cost}") ++
      Vector(s"Income ${possibility.income}")).mkString("\n")
  }
}
