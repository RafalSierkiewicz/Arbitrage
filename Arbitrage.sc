interp.configureCompiler(_.settings.Ydelambdafy.tryToSetColon(List("inline")))
import $ivy.`org.http4s::http4s-blaze-client:0.21.5`, org.http4s.client.blaze._, org.http4s.client._

import $file.DataReader
import $file.BFv2
import $file.BF_lib
import $file.PriceApi, PriceApi._
import cats.effect._
import scala.concurrent.duration._
val testCurr = DataReader.readUnsafe("data/example.json")
implicit val CS = IO.contextShift(scala.concurrent.ExecutionContext.global)

println(s"==== BFv2 ====")
new BFv2.BF(BFv2.Graph.fromApiMap(testCurr)).arbitrage.foreach(_.scopedPrint)

println(s"==== Lib ====")
new BF_lib.BF(BF_lib.Graph.fromApiMap(testCurr)).arbitrage("USD")

val client = BlazeClientBuilder[IO](scala.concurrent.ExecutionContext.global).resource.map { client =>
  IO.pure(new PriceApiClient(client, "https://fx.priceonomics.com/v1/rates/"))
}.allocated

for (i <- 0 until 1) {
  val currencies = client.flatMap(_._1.flatMap(_.getPrices)).unsafeRunTimed(1.minute).get
  println(s"==== BFv2 ====")
  new BFv2.BF(BFv2.Graph.fromApiMap(currencies)).arbitrage("USD").foreach(_.scopedPrint)

  println(s"==== Lib ====")
  new BF_lib.BF(BF_lib.Graph.fromApiMap(currencies)).arbitrage("USD")
}

client.flatMap(_._2).unsafeRunSync()
