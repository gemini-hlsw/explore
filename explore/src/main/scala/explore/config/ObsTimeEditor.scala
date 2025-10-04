// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.components.DatePicker24HTime
import explore.components.HelpIcon
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
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.ui.format.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

import java.time.Instant
import java.util.concurrent.TimeUnit

import scalajs.js
import scalajs.js.JSConverters.*

case class ObsTimeEditor(
  obsTimeView:            View[Instant],
  obsDurationView:        View[Option[TimeSpan]],
  obsTimeAndDurationView: View[(Instant, Option[TimeSpan])],
  calcDigest:             CalculatedValue[Option[ExecutionDigest]],
  forMultipleObs:         Boolean
) extends ReactFnProps(ObsTimeEditor):
  val pendingTime: Option[TimeSpan] = calcDigest.remainingObsTime.value
  val setupTime: Option[TimeSpan]   = calcDigest.fullSetupTime.value
  val timesAreLoading: Boolean      = calcDigest.isStale
  val isReadonly: Boolean           = forMultipleObs || timesAreLoading

object ObsTimeEditor
    extends ReactFnComponent[ObsTimeEditor](props =>
      for {
        defaultDuration <- useMemo(props.pendingTime)(_.getOrElse(TimeSpan.fromHours(1).get))
      } yield
        val nowTooltip         =
          s"Set time to the current time${props.pendingTime
              .fold("")(_ => " and duration to remaining time")}"
        val staleTooltipString = props.calcDigest.staleTooltipString

        <.div(ExploreStyles.ObsInstantTileTitle)(
          React.Fragment(
            <.label(
              dataAbbrv := "Time",
              <.span("Time/Duration"),
              HelpIcon("configuration/obstime.md".refined)
            ),
            DatePicker24HTime(
              props.obsTimeView,
              props.isReadonly,
              DatePicker24HTime
                .OnNow(props.pendingTime.fold(props.obsTimeView.set(Instant.now))(pt =>
                         props.obsTimeAndDurationView.set(Instant.now, pt.some)
                       ),
                       nowTooltip
                )
                .some
            ),
            <.label(ExploreStyles.TargetTileObsUTC, "UTC"),
            if (props.forMultipleObs) EmptyVdom
            else
              props.obsDurationView
                .mapValue((v: View[TimeSpan]) =>
                  val tooltipList               = List(
                    props.setupTime
                      .map(st => s"Must be > the setup time of ${formatDurationHours(st)}."),
                    props.pendingTime
                      .map(pt => s"The current remaining time is ${formatDurationHours(pt)}.")
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
