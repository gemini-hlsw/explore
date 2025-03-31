// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.Execution
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnComponent
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
import scalajs.js.JSConverters.*

case class ObsTimeEditor(
  obsTimeView:            View[Option[Instant]],
  obsDurationView:        View[Option[TimeSpan]],
  obsTimeAndDurationView: View[(Option[Instant], Option[TimeSpan])],
  execution:              Pot[Option[Execution]],
  forMultipleObs:         Boolean
) extends ReactFnProps(ObsTimeEditor):
  val optExecution: Option[Execution] = execution.toOption.flatten
  val pendingTime: Option[TimeSpan]   = optExecution.flatMap(_.remainingObsTime)
  val setupTime: Option[TimeSpan]     = optExecution.flatMap(_.fullSetupTime)
  val timesAreLoading: Boolean        = execution.isPending
  val isReadonly: Boolean             = forMultipleObs || timesAreLoading

object ObsTimeEditor
    extends ReactFnComponent[ObsTimeEditor](props =>
      for {
        ref             <- useRef[Option[ReactDatePicker[Any, Any]]](none)
        defaultDuration <- useMemo(props.pendingTime)(_.getOrElse(TimeSpan.fromHours(1).get))
      } yield
        val nowTooltip =
          s"Set time to the current time${props.pendingTime.fold("")(_ => " and duration to remaining time")}"

        <.div(ExploreStyles.ObsInstantTileTitle)(
          React.Fragment(
            <.label(
              dataAbbrv := "Time",
              <.span("Time/Duration"),
              HelpIcon("configuration/obstime.md".refined)
            ),
            Datepicker(onChange =
              (newValue, _) =>
                newValue.fromDatePickerToInstantOpt.foldMap: i =>
                  props.obsTimeView.set(i.some)
            )
              .readOnly(props.isReadonly)
              .calendarClassName(ExploreStyles.DatePickerWithNowButton.htmlClass)
              .showTimeInput(true)
              .selected(props.obsTimeView.get.getOrElse(Instant.now).toDatePickerJsDate)
              .dateFormat("yyyy-MM-dd HH:mm")(
                Button(
                  onClick = props.pendingTime.fold(props.obsTimeView.set(Instant.now.some))(pt =>
                    props.obsTimeAndDurationView.set(Instant.now.some, pt.some)
                  ) >>
                    ref.value.map(r => Callback(r.setOpen(false))).orEmpty,
                  tooltip = nowTooltip
                )("Now")
              )
              .withRef(r => ref.set(r.some).runNow()),
            <.label(ExploreStyles.TargetTileObsUTC, "UTC"),
            if (props.forMultipleObs) EmptyVdom
            else
              props.obsDurationView
                .mapValue((v: View[TimeSpan]) =>
                  val tooltipList               = List(
                    props.setupTime
                      .map(st => s"Must be > the setup time of ${st.format}."),
                    props.pendingTime
                      .map(pt => s"The current remaining time is ${pt.format}.")
                  ).flattenOption
                  val tooltip: Option[VdomNode] =
                    if (tooltipList.isEmpty) none
                    else
                      (tooltipList.mkString(
                        "",
                        "\n",
                        "\nValues entered will be coerced to this range."
                      ): VdomNode).some

                  <.span(
                    ExploreStyles.TargetTileObsDuration,
                    FormTimeSpanInput(
                      id = "obsDuration".refined,
                      value = v,
                      units = NonEmptyList.of(TimeUnit.HOURS, TimeUnit.MINUTES),
                      min = props.setupTime.map(_ +| TimeSpan.fromSeconds(1).get).orUndefined,
                      max = props.pendingTime.orUndefined,
                      tooltip = tooltip.orUndefined,
                      disabled = props.timesAreLoading
                    ),
                    Button(
                      text = true,
                      icon = Icons.ClockRotateLeft,
                      tooltip = "Set duration to full remaining sequence",
                      onClick = props.obsDurationView.set(props.pendingTime)
                    ).tiny.compact.when(
                      props.pendingTime.isDefined && props.obsDurationView.get =!= props.pendingTime
                    ),
                    props.execution.orSpinner(_ => EmptyVdom)
                  )
                )
                .getOrElse(
                  React.Fragment(
                    Button(
                      icon = Icons.ClockRotateLeft,
                      onClick = props.obsDurationView.set(defaultDuration.value.some),
                      tooltip = "Set an explicit duration instead of using the remaining sequence",
                      clazz = ExploreStyles.TargetTileObsDuration
                    ).tiny.compact,
                    props.pendingTime
                      .map(pt =>
                        <.div(ExploreStyles.TargetObsDefaultDuration, timeDisplay("", pt, ""))
                      )
                  )
                )
          )
        )
    )
