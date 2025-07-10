// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import crystal.react.View
import explore.components.ui.ExploreStyles
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.*
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.typed.reactDatepicker.mod.ReactDatePicker
import lucuma.ui.syntax.all.given
import monocle.Lens

import java.time.Instant
import java.time.LocalTime
import java.time.ZoneOffset

import scalajs.js

case class DatePicker24HTime(
  obsTimeView:   View[Instant],
  isReadonly:    Boolean,
  withNowButton: Option[DatePicker24HTime.OnNow] = None,
  minDate:       Option[Instant] = None,
  maxDate:       Option[Instant] = None
) extends ReactFnProps(DatePicker24HTime)

object DatePicker24HTime
    extends ReactFnComponent[DatePicker24HTime](props =>
      useRef[Option[ReactDatePicker[Any, Any]]](none).map: ref =>
        val localTimeView: View[LocalTime] = props.obsTimeView.zoom(instantToLocalTime)

        val datePicker = Datepicker(
          onChange =
            (newValue, _) => newValue.fromDatePickerToInstantOpt.foldMap(props.obsTimeView.set)
        )
          .readOnly(props.isReadonly)
          .calendarClassName(ExploreStyles.DatePicker.htmlClass)
          .showTimeInput(true)
          .selected(props.obsTimeView.get.toDatePickerJsDate)
          .customTimeInput(
            Time24HInputView("obs-time-input".refined,
                             localTimeView,
                             units = "UTC",
                             groupClass = ExploreStyles.DatePickerTimeEditor
            )
          )
          .dateFormat("yyyy-MM-dd HH:mm")(
            props.withNowButton.map: onNow =>
              Button(
                onClick = onNow.callback >>
                  ref.value.map(r => Callback(r.setOpen(false))).orEmpty,
                tooltip = onNow.tooltip,
                disabled = props.isReadonly
              )("Now")
          )
          .withRef(r => ref.set(r.some).runNow())

        val minDate = props.minDate.fold(datePicker)(d => datePicker.minDate(d.toDatePickerJsDate))
        props.maxDate.fold(minDate)(d => minDate.maxDate(d.toDatePickerJsDate))
    ) {
  case class OnNow(callback: Callback, tooltip: String)
}

val instantToLocalTime: Lens[Instant, LocalTime] =
  Lens[Instant, LocalTime](i => i.atZone(ZoneOffset.UTC).toLocalTime())(lt =>
    i =>
      i.atZone(ZoneOffset.UTC)
        .withHour(lt.getHour)
        .withMinute(lt.getMinute)
        .withSecond(lt.getSecond)
        .withNano(lt.getNano)
        .toInstant
  )
