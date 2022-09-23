// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.auto.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.refined.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.datepicker.*
import react.semanticui.sizes.*

import java.time.Instant

import scalajs.js
import scalajs.js.|

case class VizTimeEditor(vizTimeView: Pot[View[Option[Instant]]])
    extends ReactFnProps(VizTimeEditor.component)

object VizTimeEditor {
  private type Props = VizTimeEditor

  // TODO Move these to react-datetime
  extension (instant: Instant)
    // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
    // See https://github.com/Hacker0x01/react-datepicker/issues/1787
    def toDatePickerJsDate: js.Date =
      new js.Date(instant.toEpochMilli.toDouble + (new js.Date()).getTimezoneOffset() * 60000)

  extension [A](value: js.UndefOr[DateOrRange])
    def fromDatePickerToInstantEitherOpt(using
      A <:< js.Date
    ): Option[Either[(Instant, Instant), Instant]] =
      value.toEitherOpt.map { (e: Either[(js.Date, js.Date), js.Date]) =>
        e match {
          case Left((d1, d2)) =>
            Left((InstantBuilder.fromDatePickerJsDate(d1), InstantBuilder.fromDatePickerJsDate(d2)))
          case Right(d)       =>
            Right(InstantBuilder.fromDatePickerJsDate(d))
        }
      }.widen

    def fromDatePickerToInstantOpt(using ev: A <:< js.Date): Option[Instant] =
      fromDatePickerToInstantEitherOpt.flatMap(_.toOption)

  object InstantBuilder {
    // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
    // See https://github.com/Hacker0x01/react-datepicker/issues/1787
    def fromDatePickerJsDate(jsDate: js.Date): Instant =
      Instant.ofEpochMilli((jsDate.getTime() - jsDate.getTimezoneOffset() * 60000).toLong)
  }

  private val component =
    ScalaFnComponent[Props] { p =>
      <.div(
        ExploreStyles.ObsInstantTileTitle,
        potRender[View[Option[Instant]]](
          pendingRender = EmptyVdom,
          valueRender = instant =>
            React.Fragment(
              <.label(dataAbbrv := "Time",
                      <.span("Observation time"),
                      HelpIcon("configuration/obstime.md".refined)
              ),
              Datepicker(onChange =
                (newValue, _) =>
                  newValue.fromDatePickerToInstantOpt.foldMap { i =>
                    instant.set(i.some)
                  }
              )
                .showTimeInput(true)
                .selected(instant.get.getOrElse(Instant.now).toDatePickerJsDate)
                .dateFormat("yyyy-MM-dd HH:mm"),
              <.label("UTC")
            )
        )(p.vizTimeView)
      )
    }

}
