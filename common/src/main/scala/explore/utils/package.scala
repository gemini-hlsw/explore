// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Applicative
import cats.Endo
import cats.Monad
import cats.Monoid
import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import clue.data.*
import clue.data.syntax.*
import crystal.Pot
import crystal.PotOption
import crystal.react.View
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.BuildInfo
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.enums.ExecutionEnvironment
import explore.model.enums.ExecutionEnvironment.Development
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.ui.enums.Theme
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.versionDateFormatter
import lucuma.ui.utils.versionDateTimeFormatter
import org.http4s.Uri
import org.scalajs.dom
import react.datepicker.*
import react.fa.IconSize
import react.primereact.Button
import react.primereact.Message
import react.primereact.MessageItem
import react.primereact.ProgressSpinner
import react.primereact.ToastRef

import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

val canvasWidth  = VdomAttr("width")
val canvasHeight = VdomAttr("height")
val dataAbbrv    = VdomAttr("data-abbrv")

def toggleReusabilityOverlay[F[_]: Sync](): F[Unit] =
  Sync[F]
    .delay(
      dom.document.body.classList.toggle(ExploreStyles.HideReusability.htmlClass)
    )
    .void

val gitHash = BuildInfo.gitHeadCommit
  .orElse(BuildInfo.herokuSourceVersion)

def version(environment: ExecutionEnvironment): NonEmptyString = {
  val instant = Instant.ofEpochMilli(BuildInfo.buildDateTime)
  NonEmptyString.unsafeFrom(
    (environment match {
      case Development => versionDateTimeFormatter.format(instant)
      case _           =>
        versionDateFormatter.format(instant) +
          "-" + gitHash.map(_.take(7)).getOrElse("NONE")
    })
      + environment.suffix
        .map(suffix => s"-$suffix")
        .orEmpty
  )
}

inline def showCount(count: Int, unit: String, plural: String): String =
  if (count == 1) s"$count $unit"
  else s"$count $plural"

inline def showCount(count: Int, unit: String): String =
  showCount(count, unit, unit + "s")

extension (uri: Uri)
  def addPath(p: Uri.Path): Uri =
    uri.withPath(uri.path.concat(p))

def forceAssign[T, S](mod: Endo[Input[S]] => Endo[T])(base: S): Endo[S] => Endo[T] =
  modS =>
    mod {
      case Assign(edit) => modS(edit).assign
      case _            => modS(base).assign
    }

// TODO Move these to lucuma-ui
extension (toastRef: ToastRef)
  def show(text: String, severity: Message.Severity = Message.Severity.Info): Callback =
    toastRef.show(
      MessageItem(
        content = <.span(Icons.InfoLight.withSize(IconSize.LG), text),
        severity = severity,
        clazz = ExploreStyles.ExploreToast
      )
    )

  def prompt(text: String, callback: Callback): Callback =
    toastRef.show(
      MessageItem(
        content = <.div(
          ExploreStyles.ExplorePromptToast,
          <.span(
            Icons.InfoLight.withSize(IconSize.LG),
            text
          ),
          Button(size = Button.Size.Small, onClick = toastRef.clear() *> callback)("Upgrade ...")
        ),
        clazz = ExploreStyles.ExploreToast,
        sticky = true
      )
    )

extension [F[_]: Sync: ToastCtx](f: F[Unit])
  def withToast(text: String, severity: Message.Severity = Message.Severity.Info): F[Unit] =
    f <* ToastCtx[F].showToast(text, severity)

extension (f:       Callback)
  def showToastCB(
    ctx: AppContext[IO]
  )(text: String, severity: Message.Severity = Message.Severity.Info): Callback =
    import ctx.given
    f.to[IO].withToast(text, severity).runAsync

// TODO Move these to react-datetime
extension (instant: Instant)
  // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
  // See https://github.com/Hacker0x01/react-datepicker/issues/1787
  def toDatePickerJsDate: js.Date =
    new js.Date(instant.toEpochMilli.toDouble + (new js.Date()).getTimezoneOffset() * 60000)

extension (zdt: ZonedDateTime)
  // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
  // See https://github.com/Hacker0x01/react-datepicker/issues/1787
  def toDatePickerJsDate: js.Date =
    zdt.toInstant.toDatePickerJsDate

// DatePicker only works in local timezone, so we trick it by adding the timezone offset.
// See https://github.com/Hacker0x01/react-datepicker/issues/1787
private def fromDatePickerJsDate(jsDate: js.Date): Instant =
  Instant.ofEpochMilli((jsDate.getTime() - jsDate.getTimezoneOffset() * 60000).toLong)

extension [A](value: js.UndefOr[DateOrRange])
  def fromDatePickerToInstantEitherOpt(using
    A <:< js.Date
  ): Option[Either[(Instant, Instant), Instant]] =
    value.toEitherOpt.map { (e: Either[(js.Date, js.Date), js.Date]) =>
      e match {
        case Left((d1, d2)) =>
          Left((fromDatePickerJsDate(d1), fromDatePickerJsDate(d2)))
        case Right(d)       =>
          Right(fromDatePickerJsDate(d))
      }
    }.widen

  def fromDatePickerToInstantOpt(using ev: A <:< js.Date): Option[Instant] =
    fromDatePickerToInstantEitherOpt.flatMap(_.toOption)

  def fromDatePickerToZDTOpt(using ev: A <:< js.Date): Option[ZonedDateTime] =
    fromDatePickerToInstantOpt.map(i => ZonedDateTime.ofInstant(i, ZoneOffset.UTC))

object IsExpanded extends NewType[Boolean]
type IsExpanded = IsExpanded.Type
