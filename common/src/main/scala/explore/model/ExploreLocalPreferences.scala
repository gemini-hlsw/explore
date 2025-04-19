// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.effect.Sync
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.parser.decode
import io.circe.syntax.*
import log4cats.loglevel.LogLevelLogger
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import monocle.Focus
import org.scalajs.dom.window
import typings.loglevel.mod.LogLevelDesc

// Preferences stored at the browser level
case class ExploreLocalPreferences(
  level: LogLevelDesc
)

object ExploreLocalPreferences {
  implicit val levelEnumerated: Enumerated[LogLevelDesc] =
    new Enumerated[LogLevelDesc] {
      val all = List(LogLevelDesc.TRACE,
                     LogLevelDesc.DEBUG,
                     LogLevelDesc.INFO,
                     LogLevelDesc.WARN,
                     LogLevelDesc.ERROR,
                     LogLevelDesc.SILENT
      )

      def tag(l: LogLevelDesc): String =
        if (l == LogLevelDesc.TRACE) "TRACE"
        else if (l == LogLevelDesc.DEBUG) "DEBUG"
        else if (l == LogLevelDesc.INFO) "INFO"
        else if (l == LogLevelDesc.WARN) "WARN"
        else if (l == LogLevelDesc.ERROR) "ERROR"
        else "SILENT"
    }

  val Default = ExploreLocalPreferences(LogLevelLogger.Level.INFO)

  val StorageKey   = "ExplorePreferences"
  val LevelKey     = "logLevel"
  val ObsConfigKey = "obsConfig"

  val level = Focus[ExploreLocalPreferences](_.level)

  implicit val eqExploreLocalpreferences: Eq[ExploreLocalPreferences] =
    Eq.by(_.level)

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
      (LevelKey, Json.fromString(a.level.value))
    )
  }

  implicit val oidKeyDecoder: KeyDecoder[Observation.Id] = new KeyDecoder[Observation.Id] {
    override def apply(key: String): Option[Observation.Id] = Observation.Id.parse(key)
  }

  implicit val oidKeyEncoder: KeyEncoder[Observation.Id] = new KeyEncoder[Observation.Id] {
    override def apply(foo: Observation.Id): String = foo.show

  }

  implicit val decodeAngle: Decoder[Angle] = new Decoder[Angle] {
    final def apply(c: HCursor): Decoder.Result[Angle] =
      for {
        angle <- c.downField("angle").as[Long]
      } yield Angle.fromMicroarcseconds(angle)
  }

  implicit val encodeAngle: Encoder[Angle] = new Encoder[Angle] {
    final def apply(a: Angle): Json = Json.obj(("angle", Json.fromLong(a.toMicroarcseconds)))
  }

  implicit val decoder: Decoder[ExploreLocalPreferences] = new Decoder[ExploreLocalPreferences] {
    final def apply(c: HCursor): Decoder.Result[ExploreLocalPreferences] =
      for {
        l <- c.downField(LevelKey).as[Option[String]]
      } yield ExploreLocalPreferences(levelFromString(l.orEmpty))
  }

  // Preferences at the browser level (unlike those stored in heroku)
  def loadPreferences[F[_]: Sync]: F[ExploreLocalPreferences] = Sync[F]
    .delay {
      val preferences: Option[ExploreLocalPreferences] = for {
        ls <- Option(window.localStorage)
        d  <- Option(ls.getItem(StorageKey))
        l  <- decode[ExploreLocalPreferences](d).toOption
      } yield l
      preferences.getOrElse(Default)
    }
    .handleError(_ => Default) // In errors just return the default

  def storePreferences[F[_]: Sync](p: ExploreLocalPreferences): F[Unit] = Sync[F]
    .delay {
      for {
        ls <- Option(window.localStorage)
        _  <- Option(ls.setItem(StorageKey, p.asJson.spaces2))
      } yield ()
    }
    .handleError(_ => ().some)
    .void

}
