// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Endo
import cats.Monoid
import cats.effect.Sync
import cats.syntax.all._
import clue.data._
import clue.data.syntax._
import crystal.Pot
import crystal.PotOption
import crystal.react.View
import crystal.react.reuse._
import eu.timepit.refined._
import eu.timepit.refined.types.string.NonEmptyString
import explore.BuildInfo
import explore.components.ui.ExploreStyles
import explore.model.enums.ExecutionEnvironment
import explore.model.enums.ExecutionEnvironment.Development
import explore.model.enums.Theme
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.ExternalValue
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.versionDateFormatter
import lucuma.ui.utils.versionDateTimeFormatter
import org.http4s.Uri
import org.scalajs.dom
import react.semanticui.collections.message.Message
import react.semanticui.elements.loader.Loader

import java.time.Instant
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

def setupScheme[F[_]: Sync](theme: Theme): F[Unit] =
  Sync[F].delay {
    if (theme === Theme.Dark) {
      dom.document.body.classList.add(Theme.Dark.clazz.htmlClass)
      dom.document.body.classList.remove(Theme.Light.clazz.htmlClass)
    } else {
      dom.document.body.classList.add(Theme.Light.clazz.htmlClass)
      dom.document.body.classList.remove(Theme.Dark.clazz.htmlClass)
    }
  }

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
val DefaultPendingRender: VdomNode = Loader(active = true)

val DefaultErrorRender: Throwable => VdomNode =
  t => Message(error = true)(t.getMessage)

def potRenderWithReuse[A](
  valueRender:   A ==> VdomNode,
  pendingRender: => Reuse[VdomNode] = Reuse.always(DefaultPendingRender),
  errorRender:   Throwable ==> VdomNode = Reuse.always(DefaultErrorRender)
): Pot[A] ==> VdomNode =
  (pendingRender, errorRender, valueRender).curryReusing.in(
    (pendingRender, errorRender, valueRender, pot: Pot[A]) =>
      pot.fold(pendingRender, errorRender, valueRender)
  )

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
  @inline
  def render(
    valueRender:   A => VdomNode,
    pendingRender: => VdomNode = DefaultPendingRender,
    errorRender:   Throwable => VdomNode = DefaultErrorRender
  ): VdomNode = potRender(valueRender, pendingRender, errorRender)(pot)
}

final implicit class PotOptionRenderOps[A](val po: PotOption[A]) extends AnyVal {
  @inline
  def render(
    valueRender:   A => VdomNode,
    pendingRender: => VdomNode = DefaultPendingRender,
    errorRender:   Throwable => VdomNode = DefaultErrorRender
  ): VdomNode = po.toPot.render(valueRender, pendingRender, errorRender)
}

def showCount(count: Int, unit: String, plural: String): String =
  if (count == 1) s"$count $unit"
  else s"$count $plural"

@inline def showCount(count: Int, unit: String): String =
  showCount(count, unit, unit + "s")

implicit class Http4sUriOps(val uri: Uri) extends AnyVal {
  def addPath(p: Uri.Path): Uri =
    uri.withPath(uri.path.concat(p))
}

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
  view:        EV[Option[A]]
)(implicit ev: ExternalValue[EV]): js.UndefOr[VdomNode] =
  ev.get(view)
    .flatten
    .map(_ => <.i(ExploreStyles.ClearableInputIcon, ^.onClick --> ev.set(view)(None)))
    .orUndefined

given Monoid[VdomNode] = new Monoid[VdomNode]:
  val empty: VdomNode                             = EmptyVdom
  def combine(x: VdomNode, y: VdomNode): VdomNode = React.Fragment(x, y)
