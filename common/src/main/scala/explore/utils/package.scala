// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

import scala.scalajs.js

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enum.ExecutionEnvironment
import explore.model.enum.ExecutionEnvironment.Development

package object utils extends ReactUtils {

  def abbreviate(s: String, maxLength: Int): String =
    if (s.length > maxLength) s"${s.substring(0, maxLength)}\u2026" else s

  implicit class ListOps[A](val list: List[A]) extends AnyVal {
    def modFirstWhere(find: A => Boolean, mod: A => A): List[A] =
      list.indexWhere(find) match {
        case -1 => list
        case n  => (list.take(n) :+ mod(list(n))) ++ list.drop(n + 1)
      }
  }

  private val versionDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd").withZone(ZoneId.from(ZoneOffset.UTC))

  private val versionDateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss").withZone(ZoneId.from(ZoneOffset.UTC))

  def version(environment: ExecutionEnvironment): NonEmptyString = {
    val instant = Instant.ofEpochMilli(BuildInfo.buildTime)
    NonEmptyString.unsafeFrom(
      (environment match {
        case Development => versionDateTimeFormatter.format(instant)
        case _           =>
          versionDateFormatter.format(instant) +
            "-" + BuildInfo.gitHeadCommit.map(_.takeRight(7)).getOrElse("NONE")
      })
        + environment.suffix
          .map(suffix => s"-$suffix")
          .orEmpty
    )
  }
}

package utils {

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
  }

  object ExploreEvent {
    object Logout extends ExploreEvent {
      val event = 1
    }
  }

}
