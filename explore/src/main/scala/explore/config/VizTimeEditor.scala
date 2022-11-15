// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.refined.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.datepicker.*

import lucuma.core.util.Timestamp
import java.time.Instant

import scalajs.js

case class VizTimeEditor(vizTimeView: Pot[View[Option[Timestamp]]])
    extends ReactFnProps(VizTimeEditor.component)

object VizTimeEditor {
  private type Props = VizTimeEditor

  private val component =
    ScalaFnComponent[Props] { p =>
      <.div(
        ExploreStyles.ObsInstantTileTitle,
        potRender[View[Option[Timestamp]]](
          pendingRender = EmptyVdom,
          valueRender = timestamp =>
            React.Fragment(
              <.label(dataAbbrv := "Time",
                      <.span("Observation time"),
                      HelpIcon("configuration/obstime.md".refined)
              ),
              Datepicker(onChange =
                (newValue, _) =>
                  newValue.fromDatePickerToInstantOpt.foldMap { i =>
                    timestamp.set(Timestamp.fromInstantTruncated(i))
                  }
              )
                .showTimeInput(true)
                .selected(timestamp.get.map(_.toInstant).getOrElse(Instant.now).toDatePickerJsDate)
                .dateFormat("yyyy-MM-dd HH:mm"),
              <.label("UTC")
            )
        )(p.vizTimeView)
      )
    }

}
