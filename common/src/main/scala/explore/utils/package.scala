// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Endo
import cats.effect.Sync
import cats.syntax.all._
import clue.data._
import clue.data.syntax._
import crystal.Pot
import crystal.react.View
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.model.enums.ExecutionEnvironment
import explore.model.enums.ExecutionEnvironment.Development
import explore.model.enums.Theme
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.utils.versionDateFormatter
import lucuma.ui.utils.versionDateTimeFormatter
import org.http4s.Uri
import org.scalajs.dom
import react.semanticui.collections.message.Message
import react.semanticui.elements.loader.Loader
import react.common._

import java.time.Instant

package object utils {
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

  def potRenderWithReuse[A](
    valueRender:   A ==> VdomNode,
    pendingRender: Long ==> VdomNode = Reuse.always(_ => Loader(active = true)),
    errorRender:   Throwable ==> VdomNode = Reuse.always(t => Message(error = true)(t.getMessage))
  ): Pot[A] ==> VdomNode =
    (pendingRender, errorRender, valueRender).curryReusing.in(
      (pendingRender, errorRender, valueRender, pot: Pot[A]) =>
        pot.fold(pendingRender, errorRender, valueRender)
    )

  def potRender[A](
    valueRender:   A => VdomNode,
    pendingRender: Long => VdomNode = _ => Loader(active = true),
    errorRender:   Throwable => VdomNode = t => Message(error = true)(t.getMessage)
  ): Pot[A] => VdomNode =
    _.fold(pendingRender, errorRender, valueRender)

  def potRenderView[A](
    valueRender:   A => VdomNode,
    pendingRender: Long => VdomNode = _ => Loader(active = true),
    errorRender:   Throwable => VdomNode = t => Message(error = true)(t.getMessage)
  ): View[Pot[A]] => VdomNode =
    _.get.fold(pendingRender, errorRender, valueRender)

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
}
