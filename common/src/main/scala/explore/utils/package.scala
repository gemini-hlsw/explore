// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Endo
import cats.effect.Sync
import cats.syntax.all._
import clue.data._
import clue.data.syntax._
import crystal.Pot
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.RemoteSyncUndoableF
import explore.model.enum.ExecutionEnvironment
import explore.model.enum.ExecutionEnvironment.Development
import explore.model.enum.Theme
import explore.undo.UndoContext
import japgolly.scalajs.react.util.DefaultEffects.{ Async => DefaultA }
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.utils.versionDateFormatter
import lucuma.ui.utils.versionDateTimeFormatter
import org.http4s.Uri
import org.scalajs.dom
import react.semanticui.collections.message.Message
import react.semanticui.elements.loader.Loader
import pprint.PPrinter

import java.time.Instant
import scala.scalajs.js

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

  def potRender[A](
    valueRender:   A ==> VdomNode,
    pendingRender: Long ==> VdomNode = Reuse.always(_ => Loader(active = true)),
    errorRender:   Throwable ==> VdomNode = Reuse.always(t => Message(error = true)(t.getMessage))
  ): Pot[A] ==> VdomNode =
    (pendingRender, errorRender, valueRender).curryReusing.in(
      (pendingRender, errorRender, valueRender, pot: Pot[A]) =>
        pot.fold(pendingRender, errorRender, valueRender)
    )

  def showCount(count: Int, unit: String, plural: String): String =
    if (count == 1) s"$count $unit"
    else s"$count $plural"

  @inline def showCount(count: Int, unit: String): String =
    showCount(count, unit, unit + "s")

  implicit class Http4sUriOps(val uri: Uri) extends AnyVal {
    def addPath(p: Uri.Path): Uri =
      uri.withPath(uri.path.concat(p))
  }

  type RemoteSyncUndoable[A, T] = RemoteSyncUndoableF[DefaultA, A, T]
  object RemoteSyncUndoable {
    def apply[A, T](
      undoCtx:         UndoContext[A],
      remoteBaseInput: T,
      onMod:           T => DefaultA[Unit]
    ): RemoteSyncUndoable[A, T] =
      RemoteSyncUndoableF(undoCtx, remoteBaseInput, onMod)
  }

  def forceAssign[T, S](mod: Endo[Input[S]] => Endo[T])(base: S): Endo[S] => Endo[T] =
    modS =>
      mod {
        case Assign(edit) => modS(edit).assign
        case _            => modS(base).assign
      }
}

package utils {

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
    def value: String // encode whatever value as a String. it can be e.g. json
  }

  object ExploreEvent {
    class Logout(val nonce: Long) extends ExploreEvent {
      val event = 1
      val value = nonce.toString
    }

    object Logout {
      val event                          = 1
      def apply(nonce: Long)             = new Logout(nonce)
      def unapply(l: Logout): Some[Long] = Some(l.nonce)
    }
  }
}
