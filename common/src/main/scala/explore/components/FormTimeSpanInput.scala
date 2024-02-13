// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimeSpan.given
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.ui.primereact.*

import java.util.concurrent.TimeUnit

case class FormTimeSpanInput(
  label:    String,
  value:    TimeSpan,
  units:    NonEmptyList[TimeUnit] =
    NonEmptyList.of(TimeUnit.DAYS, TimeUnit.HOURS, TimeUnit.MINUTES, TimeUnit.SECONDS),
  onBlur:   TimeSpan => Callback = _ => Callback.empty,
  onChange: TimeSpan => Callback = _ => Callback.empty,
  disabled: Boolean = false
) extends ReactFnProps(FormTimeSpanInput.component)

object FormTimeSpanInput:
  private type Props = FormTimeSpanInput
  def component = ScalaFnComponent
    .withHooks[Props]
    .useMemoBy(_.units)(props =>
      units =>
        val unitsSorted = units.distinct.sorted
        unitsSorted.map(u => u -> valueForUnit(unitsSorted, props.value, u)).toNem
    )
    .render: (props, timespanss) =>
      val size = PlSize.Small
      <.div(
        <.span(props.label),
        <.div(
          timespanss.value.toSortedMap.map { (unit, span) =>
            val inputId = NonEmptyString.unsafeFrom(s"${unit.shortName}-input")
            React.Fragment(
              InputNumber(
                id = inputId.value,
                maxFractionDigits = 0,
                disabled = props.disabled,
                value = span,
                onValueChange = e =>
                  val newValue = e.valueOption.getOrElse(0d)

                  val newTimeSpan =
                    timespanss.value
                      .updateWith(unit)(_ => newValue)
                      .toSortedMap
                      .foldLeft(TimeSpan.Zero) { case (acc, (u, t)) =>
                        acc +| TimeSpan
                          .fromMilliseconds(TimeUnit.MILLISECONDS.convert(t.toLong, u))
                          .get
                      }

                  props.onChange(newTimeSpan)
              ).withMods(size.cls),
              FormLabel(htmlFor = inputId, size = size)(unit.shortName)
            )
          }.toSeq: _*
        )
      )

  private def valueForUnit(
    units:           NonEmptyList[TimeUnit],
    timespan:        TimeSpan,
    unitToConvertTo: TimeUnit
  ): Double =
    units
      .foldLeft((none[Double], timespan.toMilliseconds.toLong)) {
        case ((Some(l), rest), _) => (l.some, rest)

        case ((None, rest), unit) =>
          val result = unit.convert(rest, TimeUnit.MILLISECONDS)
          val diff   = rest - TimeUnit.MILLISECONDS.convert(result, unit)

          if unit === unitToConvertTo then (result.toDouble.some, diff)
          else (none, diff)
      }
      ._1
      .getOrElse(0L)

  private given Order[TimeUnit] = Order.from((a, b) => b.ordinal.compareTo(a.ordinal))

  private given Reusability[TimeUnit]                                                    = Reusability.by_==
  private given reuseTimeSpan: Reusability[TimeSpan]                                     = Reusability.byEq
  private given reuseNel[A: Reusability]: Reusability[NonEmptyList[A]]                   = Reusability.by(_.toList)
  private given reuseNem[K: Reusability, A: Reusability]: Reusability[NonEmptyMap[K, A]] =
    Reusability.by(_.toNel)

  private given Display[TimeUnit] = Display.byShortName {
    case TimeUnit.NANOSECONDS  => "ns"
    case TimeUnit.MICROSECONDS => "µs"
    case TimeUnit.MILLISECONDS => "ms"
    case TimeUnit.SECONDS      => "s"
    case TimeUnit.MINUTES      => "m"
    case TimeUnit.HOURS        => "h"
    case TimeUnit.DAYS         => "d"
  }
