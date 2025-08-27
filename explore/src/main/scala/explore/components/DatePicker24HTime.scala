// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import crystal.react.View
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.*
import lucuma.react.datepicker.hooks.UseDatepickerRef.useDatepickerRef
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.ui.syntax.all.given
import monocle.Lens

import java.time.Instant
import java.time.LocalTime
import java.time.ZoneOffset

import scalajs.js
import scalajs.js.JSConverters.*

case class DatePicker24HTime(
  obsTimeView:   View[Instant],
  isReadonly:    Boolean,
  withNowButton: Option[DatePicker24HTime.OnNow] = None,
  minDate:       Option[Instant] = None,
  maxDate:       Option[Instant] = None
) extends ReactFnProps(DatePicker24HTime)

object DatePicker24HTime
    extends ReactFnComponent[DatePicker24HTime](props =>
      useDatepickerRef.map: datepickerRef =>
        val localTimeView: View[LocalTime] = props.obsTimeView.zoom(instantToLocalTime)

        val minDate = props.minDate.map(_.toDatePickerJsDate)
        val maxDate = props.maxDate.map(_.toDatePickerJsDate)

        Datepicker(
          onChange = _.map(_.fromDatePickerJsDate).foldMap(props.obsTimeView.set),
          selected = props.obsTimeView.get.toDatePickerJsDate.some,
          minDate = minDate.orUndefined,
          maxDate = maxDate.orUndefined,
          dateFormat = "yyyy-MM-dd HH:mm",
          readonly = props.isReadonly,
          calendarClassName = ExploreStyles.DatePicker,
          showTimeInput = true,
          customTimeInput = Time24HInputView("obs-time-input".refined,
                                             localTimeView,
                                             units = "UTC",
                                             groupClass = ExploreStyles.DatePickerTimeEditor
          )
        )(
          props.withNowButton.map: onNow =>
            Button(
              onClick = onNow.callback >>
                datepickerRef.setOpen(false),
              tooltip = onNow.tooltip,
              disabled = props.isReadonly
            )("Now")
        ).withRef(datepickerRef.ref)
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
