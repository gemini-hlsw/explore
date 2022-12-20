// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Endo
import cats.Monoid
import cats.effect.Sync
import cats.syntax.all.*
import clue.data.*
import clue.data.syntax.*
import crystal.Pot
import crystal.PotOption
import crystal.react.View
import crystal.react.reuse.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.BuildInfo
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.enums.ExecutionEnvironment
import explore.model.enums.ExecutionEnvironment.Development
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.ui.enums.Theme
import lucuma.ui.forms.ExternalValue
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

// TODO All the "potRender" methods should go in lucuma-ui
val DefaultPendingRender: VdomNode = ProgressSpinner(clazz = ExploreStyles.Loader)

val DefaultErrorRender: Throwable => VdomNode =
  t => Message(text = t.getMessage, severity = Message.Severity.Error)

def potRender[A](
  valueRender:   A => VdomNode,
  pendingRender: => VdomNode = DefaultPendingRender,
  errorRender:   Throwable => VdomNode = DefaultErrorRender
): Pot[A] => VdomNode =
  _.fold(pendingRender, errorRender, valueRender)

def potRenderView[A](
  valueRender:   A => VdomNode,
  pendingRender: => VdomNode = DefaultPendingRender,
  errorRender:   Throwable => VdomNode = DefaultErrorRender
): View[Pot[A]] => VdomNode =
  _.get.fold(pendingRender, errorRender, valueRender)

final implicit class PotRenderOps[A](val pot: Pot[A]) extends AnyVal {
  inline def render(
    valueRender:   A => VdomNode,
    pendingRender: => VdomNode = DefaultPendingRender,
    errorRender:   Throwable => VdomNode = DefaultErrorRender
  ): VdomNode = potRender(valueRender, pendingRender, errorRender)(pot)
}

final implicit class PotOptionRenderOps[A](val po: PotOption[A]) extends AnyVal {
  inline def render(
    valueRender:   A => VdomNode,
    pendingRender: => VdomNode = DefaultPendingRender,
    errorRender:   Throwable => VdomNode = DefaultErrorRender
  ): VdomNode = po.toPot.render(valueRender, pendingRender, errorRender)
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

/**
 * Creates an `X` icon that clears the data from a `FormInputEV` which uses a `View[Option[A]]` It
 * should be assigned to the `icon` parameter of `FormInputEV` and use the same `View`. It is styled
 * to look like the clear icon for the `EnumViewOptionalSelect`s.
 *
 * Depending on the layout containing the `FormInputEV`, you may also need to add the
 * `ExploreStyles.ClearableInputPaddingReset` class. When an icon is added to a FormInput, it
 * changes the padding for the enclosed `input` to make room for the icon - lots of room - which can
 * change your layout. The above class resets the padding to the standard for FormInputs without
 * icons. The downside, of course, is that if your content can extend all the wqy to the right side
 * of the input, the `X` icon will sit on top of it. Usually this is not a problem. Note that this
 * will override that for `InputWithUnits` this will override the default `clazz` of
 * `ExploreStyles.Grow(1), so you may need to add that, too.
 */
def clearInputIcon[EV[_], A](
  view:     EV[Option[A]]
)(using ev: ExternalValue[EV]): js.UndefOr[VdomNode] =
  ev.get(view)
    .flatten
    .map(_ => <.i(ExploreStyles.ClearableInputIcon, ^.onClick --> ev.set(view)(None)))
    .orUndefined

extension (toastRef: ToastRef)
  def info(text: String) =
    toastRef.show(
      MessageItem(
        content = <.span(Icons.InfoLight.withSize(IconSize.LG), text),
        clazz = ExploreStyles.ExploreToast
      )
    )

  def prompt(text: String, callback: Callback) =
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

// TODO Move these to react-datetime
extension (instant:  Instant)
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
