// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.*
import lucuma.refined.*
import lucuma.ui.syntax.all.given

import java.time.Instant

import scalajs.js

case class VizTimeEditor(vizTimeView: View[Option[Instant]])
    extends ReactFnProps(VizTimeEditor.component)

object VizTimeEditor {
  private type Props = VizTimeEditor

  private val component =
    ScalaFnComponent[Props] { props =>
      <.div(ExploreStyles.ObsInstantTileTitle)(
        React.Fragment(
          <.label(
            dataAbbrv := "Time",
            <.span("Observation time"),
            HelpIcon("configuration/obstime.md".refined)
          ),
          Datepicker(onChange =
            (newValue, _) =>
              newValue.fromDatePickerToInstantOpt.foldMap { i =>
                props.vizTimeView.set(i.some)
              }
          )
            .showTimeInput(true)
            .selected(props.vizTimeView.get.getOrElse(Instant.now).toDatePickerJsDate)
            .dateFormat("yyyy-MM-dd HH:mm"),
          <.label("UTC")
        )
      )
    }

}
