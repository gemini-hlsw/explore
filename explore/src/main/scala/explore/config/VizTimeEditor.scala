// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import crystal.react.View
import eu.timepit.refined.auto._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.datepicker._

import java.time.Instant

import scalajs.js
import scalajs.js.|

final case class VizTimeEditor(instant: View[Option[Instant]])
    extends ReactFnProps[VizTimeEditor](VizTimeEditor.component)

object VizTimeEditor {
  type Props = VizTimeEditor

  // TODO Move these to react-datetime
  implicit class InstantOps(val instant: Instant) extends AnyVal {
    // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
    // See https://github.com/Hacker0x01/react-datepicker/issues/1787
    def toDatePickerJsDate: js.Date =
      new js.Date(instant.toEpochMilli.toDouble + (new js.Date()).getTimezoneOffset() * 60000)
  }

  object InstantBuilder {
    // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
    // See https://github.com/Hacker0x01/react-datepicker/issues/1787
    def fromDatePickerJsDate(jsDate: js.Date): Instant =
      Instant.ofEpochMilli((jsDate.getTime() - jsDate.getTimezoneOffset() * 60000).toLong)
  }

  implicit class JSUndefOrNullOrTuple2DateTimeOps[A](
    val value: js.UndefOr[DateOrRange]
  ) extends AnyVal {
    def toEitherOpt2: Option[Either[(A, A), A]] =
      value.toOption
        .flatMap(valueOrNull => Option(valueOrNull.asInstanceOf[A | js.Tuple2[A, A]]))
        .map { valueOrTuple =>
          if (js.Array.isArray(valueOrTuple))
            Left(valueOrTuple.asInstanceOf[js.Tuple2[A, A]])
          else
            Right(valueOrTuple.asInstanceOf[A])
        }

    def fromDatePickerToInstantEitherOpt(implicit
      ev: A <:< js.Date
    ): Option[Either[(Instant, Instant), Instant]] =
      value.toEitherOpt.map { (e: Either[(js.Date, js.Date), js.Date]) =>
        e match {
          case Left((d1, d2)) =>
            Left((InstantBuilder.fromDatePickerJsDate(d1), InstantBuilder.fromDatePickerJsDate(d2)))
          case Right(d)       =>
            Right(InstantBuilder.fromDatePickerJsDate(d))
        }
      }.widen

    def fromDatePickerToInstantOpt(implicit ev: A <:< js.Date): Option[Instant] =
      fromDatePickerToInstantEitherOpt.flatMap(_.toOption)
  }

  val component =
    ScalaFnComponent[Props] { p =>
      <.div(
        ExploreStyles.ObsConfigurationObsTime,
        <.label("Observation time", HelpIcon("configuration/obstime.md")),
        Datepicker(onChange =
          (newValue, _) =>
            newValue.fromDatePickerToInstantOpt.foldMap { i =>
              p.instant.set(i.some)
            }
        )
          .showTimeInput(true)
          .selected(p.instant.get.getOrElse(Instant.now).toDatePickerJsDate)
          .dateFormat("yyyy-MM-dd HH:mm"),
        "UTC"
      )
    }
}
