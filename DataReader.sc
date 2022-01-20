import $ivy.`io.circe::circe-core:0.14.1`, io.circe._
import $ivy.`io.circe::circe-generic:0.14.1`, io.circe.generic._
import $ivy.`io.circe::circe-parser:0.14.1`, io.circe.parser._, io.circe.syntax._

import scala.util.Try
import java.nio.file._
import java.io.File

def read(path: String) =
  readFile(path).flatMap(parser.parse).flatMap(_.as[Map[String, Double]])

private def readFile(path: String) =
  Try(Files.readAllBytes(Paths.get(path))).map(new String(_)).toEither
