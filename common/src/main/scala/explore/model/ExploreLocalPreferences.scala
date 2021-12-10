// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.effect.Sync
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax._
import log4cats.loglevel.LogLevelLogger
import monocle.Focus
import org.scalajs.dom.window
import typings.loglevel.mod.LogLevelDesc

// Preferences stored at the browser level
case class ExploreLocalPreferences(level: LogLevelDesc)

object ExploreLocalPreferences {
  val Default = ExploreLocalPreferences(LogLevelLogger.Level.DEBUG)
  val level   = Focus[ExploreLocalPreferences](_.level)

  implicit val eqExploreLocalpreferences: Eq[ExploreLocalPreferences] = Eq.by(_.level.toString)

  implicit class LevelOps(val l: LogLevelDesc) extends AnyVal {
    def value: String =
      if (l == LogLevelDesc.TRACE) "TRACE"
      else if (l == LogLevelDesc.DEBUG) "DEBUG"
      else if (l == LogLevelDesc.INFO) "INFO"
      else if (l == LogLevelDesc.WARN) "WARN"
      else if (l == LogLevelDesc.ERROR) "ERROR"
      else "SILENT"
  }

  def levelFromString(s: String): LogLevelDesc = s match {
    case "TRACE"  => LogLevelDesc.TRACE
    case "DEBUG"  => LogLevelDesc.DEBUG
    case "WARN"   => LogLevelDesc.WARN
    case "ERROR"  => LogLevelDesc.ERROR
    case "SILENT" => LogLevelDesc.SILENT
    case _        => LogLevelDesc.INFO
  }

  implicit val encoder: Encoder[ExploreLocalPreferences] = new Encoder[ExploreLocalPreferences] {
    final def apply(a: ExploreLocalPreferences): Json = Json.obj(
      ("logLevel", Json.fromString(a.level.value))
    )
  }

  implicit val decodeFoo: Decoder[ExploreLocalPreferences] = new Decoder[ExploreLocalPreferences] {
    final def apply(c: HCursor): Decoder.Result[ExploreLocalPreferences] =
      for {
        l <- c.downField("logLevel").as[Option[String]]
      } yield ExploreLocalPreferences(levelFromString(l.orEmpty))
  }

  // Preferences at the browser level (unlike those stored in heroku)
  def loadPreferences[F[_]: Sync]: F[ExploreLocalPreferences] = Sync[F]
    .delay {
      val preferences: Option[ExploreLocalPreferences] = for {
        ls <- Option(window.localStorage)
        d  <- Option(ls.getItem("ExplorePreferences"))
        l  <- decode[ExploreLocalPreferences](d).toOption
      } yield l
      preferences.getOrElse(Default)
    }
    .handleErrorWith(_ => storePreferences[F](Default).as(Default))

  def storePreferences[F[_]: Sync](p: ExploreLocalPreferences): F[Unit] = Sync[F]
    .delay {
      for {
        ls <- Option(window.localStorage)
        _  <- Option(ls.setItem("ExplorePreferences", p.asJson.spaces2))
      } yield println(p.asJson.spaces2)
    }
    .handleError(_ => ().some)
    .void
}
