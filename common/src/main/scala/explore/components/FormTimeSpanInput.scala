// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimeSpan.given
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*

import java.util.concurrent.TimeUnit
import scala.collection.immutable.SortedMap

case class FormTimeSpanInput(
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
    .useMemoBy(props => (props.units, props.value))(_ => makeTimeUnitsMap(_, _))
    .render: (props, timeUnitValues) =>
      <.div(
        ^.id := s"form-time-span-input",
        timeUnitValues.value.toVdomArray: (unit, value) =>
          val inputId = s"${unit.shortName}-input"
          InputNumber(
            id = inputId,
            maxFractionDigits = 0,
            disabled = props.disabled,
            value = value,
            suffix = unit.shortName,
            min = 0,
            onValueChange = e =>
              props.onChange(
                // Calculate the total timespan from the individual time units
                timeUnitValues.value
                  .updated(unit, e.valueOption.getOrElse(0d))
                  .foldLeft(TimeSpan.Zero) { case (acc, (unit, value)) =>
                    acc +| TimeSpan.unsafeFromMicroseconds(
                      TimeUnit.MICROSECONDS.convert(value.toLong, unit)
                    )

                  }
              )
          ).withMods(^.key := unit.shortName, ExploreStyles.TimeSpanInputItem)
      )

  /**
   * Create a map of time units and their values for the given timespan
   *
   * E.g. a timespan of 25.5 hours would be 1 (day), 1 (hour), 30 (minutes)
   */
  def makeTimeUnitsMap(
    units: NonEmptyList[TimeUnit],
    value: TimeSpan
  ): SortedMap[TimeUnit, Double] =
    units.distinct.sorted
      .foldLeft((SortedMap.empty[TimeUnit, Double], value.toMicroseconds)):
        case ((acc, rest), unit) =>
          val result = unit.convert(rest, TimeUnit.MICROSECONDS)
          val diff   = rest - TimeUnit.MICROSECONDS.convert(result, unit)

          (acc + (unit -> result.toDouble), diff)
      ._1

  // Custom ordering to go from biggest to smallest
  private given Ordering[TimeUnit] = Ordering.by[TimeUnit, Int](_.ordinal).reverse
  private given Order[TimeUnit]    = Order.fromOrdering

  private given Reusability[TimeSpan]                 = Reusability.byEq
  private given [A: Eq]: Reusability[NonEmptyList[A]] = Reusability.byEq

  private given Display[TimeUnit] = Display.byShortName {
    case TimeUnit.NANOSECONDS  => "ns"
    case TimeUnit.MICROSECONDS => "µs"
    case TimeUnit.MILLISECONDS => "ms"
    case TimeUnit.SECONDS      => "s"
    case TimeUnit.MINUTES      => "m"
    case TimeUnit.HOURS        => "h"
    case TimeUnit.DAYS         => "d"
  }
