// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.fa.IconSize
import react.semanticui.modules.popup.Popup

import java.time.Duration
import lucuma.utils.NewType

// Icon to indicate a field is required to do ITC calculations
def requiredForITC: TagMod =
  Popup(
    trigger = <.span(
      ^.cls := "fa-layers fa-fw",
      Icons.StarExclamation
        .clazz(ExploreStyles.WarningIcon)
        .size(IconSize.X1),
      <.span(^.cls := "fa-layers-text fa-inverse", "ITC")
    )
  )("Required for ITC")

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
