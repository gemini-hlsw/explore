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
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.typed.reactDatepicker.mod.ReactDatePicker
import lucuma.ui.syntax.all.given

import java.time.Instant

import scalajs.js

case class ObsTimeEditor(vizTimeView: View[Option[Instant]])
    extends ReactFnProps(ObsTimeEditor.component)

object ObsTimeEditor {
  private type Props = ObsTimeEditor

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useRef[Option[ReactDatePicker[Any, Any]]](none)
      // .useRefToJsComponentWithMountedFacade[js.Object, js.Object, js.Object]
      .render: (props, ref) =>
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
              .calendarClassName(ExploreStyles.DatePickerWithNowButton.htmlClass)
              .showTimeInput(true)
              .selected(props.vizTimeView.get.getOrElse(Instant.now).toDatePickerJsDate)
              .dateFormat("yyyy-MM-dd HH:mm")(
                Button(onClick =
                  props.vizTimeView.set(Instant.now.some) >>
                    ref.value.map(r => Callback(r.setOpen(false))).orEmpty
                )("Now")
              )
              .withRef(r => ref.set(r.some).runNow()),
            <.label("UTC")
          )
        )
}
