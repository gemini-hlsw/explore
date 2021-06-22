// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats._
import cats.effect.SyncIO
import cats.effect.std.Dispatcher
import cats.syntax.all._
import clue._
import coulomb.Quantity
import crystal.ViewF
import crystal.ViewOptF
import explore.model.AppContext
import explore.model.RootModel
import explore.optics._
import explore.schemas._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom._
import lucuma.ui.utils._
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import shapeless._

import scala.annotation.unused
import scala.concurrent.duration

trait ListImplicits {

  // Adapted from https://stackoverflow.com/a/21444327/5808801
  implicit object hnilMonoid extends Monoid[HNil] {
    val empty = HNil
    def combine(@unused f1: HNil, @unused f2: HNil) = HNil
  }

  implicit def hconsMonoid[H: Monoid, T <: HList: Monoid] =
    new Monoid[H :: T] {
      val empty = Monoid[H].empty :: Monoid[T].empty
      def combine(f1: H :: T, f2: H :: T) =
        (f1.head |+| f2.head) :: (f1.tail |+| f2.tail)
    }

  private object singleton extends Poly1 { implicit def anything[A] = at[A](List(_)) }

  implicit class UnzipListOpts[L <: HList](hlists: List[L]) {
    def unzipN[Out <: HList](implicit
      mapper: ops.hlist.Mapper.Aux[singleton.type, L, Out],
      monoid: Monoid[Out]
    ): Out = hlists.map(_.map(singleton)).combineAll
  }

  implicit class ViewListOps[F[_], A](val viewList: ViewF[F, List[A]]) {
    def toListOfViews[B](eqBy: A => B)(implicit eq: Eq[B]): List[ViewF[F, A]] =
      viewList.get.map { a =>
        // We're already focused on "this" element
        val getA: List[A] => A = _ => a
        def modA(mod: A => A): List[A] => List[A] =
          list => list.modFirstWhere(thisA => eqBy(thisA) === eqBy(a), mod)

        viewList.zoom[A](getA)(modA)
      }
  }
}

trait ContextImplicits {
  implicit def appContext2Dispatcher[F[_]](implicit ctx: AppContext[F]): Dispatcher[F] =
    ctx.dispatcher
  implicit def appContext2Logger[F[_]](implicit ctx:     AppContext[F]): Logger[F]     =
    ctx.logger
  implicit def appContext2UserPreferencesDBClient[F[_]](implicit
    ctx:                                                 AppContext[F]
  ): WebSocketClient[F, UserPreferencesDB] =
    ctx.clients.preferencesDB
  implicit def appContext2ODBClient[F[_]](implicit
    ctx: AppContext[F]
  ): WebSocketClient[F, ObservationDB] =
    ctx.clients.odb
  implicit def appContext2fromSync[F[_]](implicit
    ctx: AppContext[F]
  ): SyncIO ~> F = ctx.fromSyncIO
}

object implicits extends ShorthandTypes with ListImplicits with ContextImplicits {
  // View Optics implicits
  implicit class ViewOpticsOps[F[_], A](val view: ViewF[F, A]) extends AnyVal {
    def zoomGetAdjust[B](getAdjust: GetAdjust[A, B]): ViewF[F, B] =
      view.zoom(getAdjust.get)(getAdjust.mod)

    // Helps type inference by sidestepping overloaded "zoom".
    def zoomPrism[B](prism: monocle.Prism[A, B]): ViewOptF[F, B] =
      view.zoom(prism)

    // Helps type inference by sidestepping overloaded "zoom".
    def zoomLens[B](lens: monocle.Lens[A, B]): ViewF[F, B] =
      view.zoom(lens)
  }

  implicit class ViewOptOpticsOps[F[_], A](val viewOpt: ViewOptF[F, A]) extends AnyVal {
    // Helps type inference by sidestepping overloaded "zoom".
    def zoomLens[B](lens: monocle.Lens[A, B]): ViewOptF[F, B] =
      viewOpt.zoom(lens)
  }

  // Coulomb implicits
  implicit class CoulombViewOps[F[_], N, U](val self: ViewF[F, Quantity[N, U]]) extends AnyVal {
    def stripQuantity: ViewF[F, N] = self.as(coulombIso[N, U])
  }

  implicit class CoulombViewOptOps[F[_], N, U](val self: ViewOptF[F, Quantity[N, U]])
      extends AnyVal {
    def stripQuantity: ViewOptF[F, N] = self.as(coulombIso[N, U])
  }

  // Model implicits
  implicit class RootModelOps(val rootModel: RootModel) extends AnyVal {
    def url[F[_]](implicit ctx: AppContext[F]): String =
      ctx.pageUrl(rootModel.tabs.focus, rootModel.focused)
  }

  // React implicits
  implicit class HtmlAttrsOps(val a: HtmlAttrs) extends AnyVal {
    // Generalize https://gist.github.com/pstoica/4323d3e6e37e8a23dd59
    def onComponentBlur(handler: Callback): TagMod =
      a.onBlur ==> { (e: ReactFocusEvent) =>
        val currentTarget = e.currentTarget

        handler
          .when_(!currentTarget.contains(dom.document.activeElement))
          .setTimeout(duration.Duration.Zero)
          .void
      }
  }
}
