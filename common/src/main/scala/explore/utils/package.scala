// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import java.time.Instant

import scala.scalajs.js

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enum.ExecutionEnvironment
import explore.model.enum.ExecutionEnvironment.Development
import lucuma.ui.utils.versionDateFormatter
import lucuma.ui.utils.versionDateTimeFormatter

package object utils {
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
