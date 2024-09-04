// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.*
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.typed.reactDatepicker.mod.ReactDatePicker
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

import java.time.Instant
import java.util.concurrent.TimeUnit

import scalajs.js

case class ObsTimeEditor(
  vizTimeView:     View[Option[Instant]],
  vizDurationView: View[Option[TimeSpan]],
  pendingTime:     Option[TimeSpan],
  forMultipleObs:  Boolean
) extends ReactFnProps(ObsTimeEditor.component)

object ObsTimeEditor {
  private type Props = ObsTimeEditor

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useRef[Option[ReactDatePicker[Any, Any]]](none)
      .useMemoBy((props, _) => props.pendingTime)((_, _) => _.getOrElse(TimeSpan.fromHours(1).get))
      .render: (props, ref, defaultDuration) =>
        println(s"Pending: ${props.pendingTime}")
        <.div(ExploreStyles.ObsInstantTileTitle)(
          React.Fragment(
            <.label(
              dataAbbrv := "Time",
              <.span("Observation time"),
              HelpIcon("configuration/obstime.md".refined)
            ),
            Datepicker(onChange =
              (newValue, _) =>
                newValue.fromDatePickerToInstantOpt.foldMap: i =>
                  props.vizTimeView.set(i.some)
            )
              .readOnly(props.forMultipleObs)
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
            <.label("UTC"),
            <.span(
              ExploreStyles.TargetTileObsDuration,
              if (props.forMultipleObs) TagMod.empty
              else
                props.vizDurationView
                  .mapValue((v: View[TimeSpan]) =>
                    <.span(
                      ExploreStyles.TargetTileObsDuration,
                      FormTimeSpanInput(id = "obsDuration".refined,
                                        value = v,
                                        units = NonEmptyList.of(TimeUnit.HOURS, TimeUnit.MINUTES)
                      ),
                      Button(
                        text = true,
                        clazz = ExploreStyles.DeleteButton,
                        icon = Icons.Eraser,
                        tooltip = "Clear explicit duration and use full remaining sequence",
                        onClick = props.vizDurationView.set(none)
                      ).tiny.compact
                    )
                  )
                  .getOrElse(
                    Button(
                      label = "Set duration",
                      onClick = props.vizDurationView.set(defaultDuration.value.some),
                      tooltip = "Set an explicit duration instead of using the remaining sequence",
                      clazz = ExploreStyles.TargetTileObsDuration
                    ).tiny.compact
                  )
            )
          )
        )
}
