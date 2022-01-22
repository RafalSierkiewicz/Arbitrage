import $ivy.`org.http4s::http4s-blaze-client:0.21.5`, org.http4s.client.blaze._, org.http4s.client._
import $ivy.`org.http4s::http4s-circe:0.21.5`, org.http4s.circe._

import scala.concurrent.ExecutionContext
import org.http4s.EntityDecoder
import cats.Monad
import cats.effect.Concurrent

class PriceApiClient[F[_]: Concurrent](client: Client[F], url: String) {
  implicit val decoder: EntityDecoder[F, Map[String, Double]] = jsonOf[F, Map[String, Double]]

  def getPrices: F[Map[String, Double]] = {
    client.expect[Map[String, Double]](url)
  }
}
