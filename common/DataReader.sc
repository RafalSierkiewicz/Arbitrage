import $ivy.`io.circe::circe-core:0.14.1`, io.circe._
import $ivy.`io.circe::circe-generic:0.14.1`, io.circe.generic._
import $ivy.`io.circe::circe-parser:0.14.1`, io.circe.parser._, io.circe.syntax._

import scala.util.Try
import java.nio.file._
import java.io.File

def read(file: File) =
  readFile(file.getAbsolutePath()).flatMap(parser.parse).flatMap(_.as[Map[String, Double]])

def readUnsafe(path: String) =
  read(new File(path)).left.map(throw _).merge

private def readFile(path: String) =
  Try(Files.readAllBytes(Paths.get(path))).map(new String(_)).toEither
