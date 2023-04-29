// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.NonNegInt
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcCcd
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.fa.IconSize
import react.floatingui.syntax.*

import java.time.Duration

// Icon to indicate a field is required to do ITC calculations
def requiredForITC: TagMod =
  <.span(
    ^.cls := "fa-layers fa-fw",
    Icons.StarExclamation
      .withClass(ExploreStyles.WarningIcon)
      .withSize(IconSize.X1),
    <.span(^.cls := "fa-layers-text fa-inverse", "ITC")
  ).withTooltip("Required for ITC")

def formatDuration(ts: TimeSpan): String =
  val seconds = ts.toSeconds
  if (seconds < 60)
    f"$seconds%.0f sec"
  else if (seconds < 3600)
    f"${seconds / 60.0}%.2f min"
  else
    f"${seconds / 3600.0}%.2f hr"

def format(time: TimeSpan, count: NonNegInt): String =
  s"$count × ${formatDuration(time)} = ${formatDuration(time *| count.value)}"

def formatCcds(
  ccds:      Option[NonEmptyList[ItcCcd]],
  extractor: NonEmptyList[ItcCcd] => String
): String =
  ccds.fold("-")(extractor)
