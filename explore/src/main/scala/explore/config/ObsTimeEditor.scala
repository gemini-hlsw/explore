// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import explore.Icons
import explore.components.HelpIcon
import explore.components.Time24HInputView
import explore.components.ui.ExploreStyles
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
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
import monocle.Lens

import java.time.Instant
import java.time.LocalTime
import java.time.ZoneOffset
import java.util.concurrent.TimeUnit

import scalajs.js
import scalajs.js.JSConverters.*

case class ObsTimeEditor(
  obsTimeView:            View[Option[Instant]],
  obsDurationView:        View[Option[TimeSpan]],
  obsTimeAndDurationView: View[(Option[Instant], Option[TimeSpan])],
  calcDigest:             CalculatedValue[Option[ExecutionDigest]],
  forMultipleObs:         Boolean
) extends ReactFnProps(ObsTimeEditor):
  val pendingTime: Option[TimeSpan] = calcDigest.remainingObsTime.value
  val setupTime: Option[TimeSpan]   = calcDigest.fullSetupTime.value
  val timesAreLoading: Boolean      = calcDigest.isStale
  val isReadonly: Boolean           = forMultipleObs || timesAreLoading
  val obsTimeOrNow                  = obsTimeView.get.getOrElse(Instant.now)

object ObsTimeEditor
    extends ReactFnComponent[ObsTimeEditor](props =>
      for {
        ref             <- useRef[Option[ReactDatePicker[Any, Any]]](none)
        // we need a local time view to make it easier to set it to now if unset upstream
        tView           <- useStateView[Option[LocalTime]](instantToLocalTime.get(props.obsTimeOrNow).some)
        defaultDuration <- useMemo(props.pendingTime)(_.getOrElse(TimeSpan.fromHours(1).get))
      } yield
        val nowTooltip         =
          s"Set time to the current time${props.pendingTime.fold("")(_ => " and duration to remaining time")}"
        val staleTooltipString = props.calcDigest.staleTooltipString

        // if obs time change we want to set the editor time time too
        // this happens when editing the time on the bar or when calling now
        val obsTimeView =
          props.obsTimeView.withOnMod(i =>
            i.map(i => tView.set(instantToLocalTime.get(i).some)).orEmpty
          )

        // if time changes on the time editor also change it on obsTimeView
        val timeView = tView.withOnMod(t =>
          props.obsTimeView.mod {
            case in @ Some(i) =>
              t match
                case Some(t) => i.atZone(ZoneOffset.UTC).`with`(t).toInstant.some
                case _       => in
            case None         => none
          }
        )

        <.div(ExploreStyles.ObsInstantTileTitle)(
          React.Fragment(
            <.label(
              dataAbbrv := "Time",
              <.span("Time/Duration"),
              HelpIcon("configuration/obstime.md".refined)
            ),
            Datepicker(
              onChange = (newValue, _) =>
                newValue.fromDatePickerToInstantOpt.foldMap: i =>
                  obsTimeView.set(i.some)
            )
              .readOnly(props.isReadonly)
              .calendarClassName(ExploreStyles.DatePickerWithNowButton.htmlClass)
              .showTimeInput(true)
              .selected(props.obsTimeOrNow.toDatePickerJsDate)
              .customTimeInput(
                Time24HInputView("obs-time-input".refined,
                                 timeView,
                                 units = "UTC",
                                 groupClass = ExploreStyles.TargetTileTimeEditor
                )
              )
              .dateFormat("yyyy-MM-dd HH:mm")(
                Button(
                  onClick = props.pendingTime.fold(obsTimeView.set(Instant.now.some))(pt =>
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
                    props.calcDigest.staleTooltip.orElse(
                      if (tooltipList.isEmpty) none
                      else
                        (tooltipList.mkString(
                          "",
                          "\n",
                          "\nValues entered will be coerced to this range."
                        ): VdomNode).some
                    )

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
                      tooltip =
                        staleTooltipString.getOrElse("Set duration to full remaining sequence"),
                      onClick = props.obsDurationView.set(props.pendingTime)
                    ).tiny.compact.when(
                      props.pendingTime.isDefined && props.obsDurationView.get =!= props.pendingTime
                    )
                  )
                )
                .getOrElse(
                  React.Fragment(
                    Button(
                      icon = Icons.ClockRotateLeft,
                      onClick = props.obsDurationView.set(defaultDuration.value.some),
                      tooltip = staleTooltipString.getOrElse(
                        "Set an explicit duration instead of using the remaining sequence"
                      ),
                      clazz = ExploreStyles.TargetTileObsDuration,
                      disabled = props.timesAreLoading
                    ).tiny.compact,
                    props.pendingTime
                      .map(pt =>
                        <.div(
                          ExploreStyles.TargetObsDefaultDuration,
                          timeDisplay("", pt, "", timeClass = props.calcDigest.staleClass)
                        )
                      )
                  )
                )
          )
        )
    )

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
