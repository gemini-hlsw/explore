// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.NonNegInt
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.itc.ItcCcd
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.NonNegDuration
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.utils.NewType
import react.fa.IconSize
import react.floatingui.Tooltip

import java.time.Duration

// Icon to indicate a field is required to do ITC calculations
def requiredForITC: TagMod =
  Tooltip(
    trigger = <.span(
      ^.cls := "fa-layers fa-fw",
      Icons.StarExclamation
        .clazz(ExploreStyles.WarningIcon)
        .size(IconSize.X1),
      <.span(^.cls := "fa-layers-text fa-inverse", "ITC")
    ),
    tooltip = "Required for ITC"
  )

opaque type PlotLoading = Boolean

object PlotLoading:
  val Loading: PlotLoading = true
  val Done: PlotLoading    = false

  inline def apply(b: Boolean): PlotLoading = b

extension (p: PlotLoading) inline def boolValue: Boolean = p

object PlotDetails extends NewType[Boolean]:
  val Shown: PlotDetails  = PlotDetails(true)
  val Hidden: PlotDetails = PlotDetails(false)

type PlotDetails = PlotDetails.Type

def formatDuration(seconds: Long): String =
  if (seconds < 60)
    s"$seconds sec"
  else if (seconds < 3600)
    f"${seconds / 60.0}%.2f min"
  else
    f"${seconds / 3600.0}%.2f hr"

def format(time: NonNegDuration, count: NonNegInt): String =
  s"$count × ${formatDuration(time.value.getSeconds())} = ${formatDuration(time.value.getSeconds() * count.value)}"

def formatCcds(ccds: Option[NonEmptyList[ItcCcd]], extractor: NonEmptyList[ItcCcd] => String) =
  ccds.fold("-")(extractor)
