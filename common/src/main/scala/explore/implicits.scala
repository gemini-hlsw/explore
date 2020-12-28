// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.annotation.unused

import cats._
import cats.effect.ContextShift
import cats.effect.Timer
import cats.syntax.all._
import clue._
import coulomb.Quantity
import crystal.ViewF
import crystal.ViewOptF
import explore.GraphQLSchemas._
import explore.model.AppContext
import explore.model.RootModel
import explore.optics._
import explore.utils._
import io.chrisdavenport.log4cats.Logger
import shapeless._

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
    def toAgGridData[B](eqBy: A => B)(implicit eq: Eq[B]): List[ViewF[F, A]] =
      viewList.get.map { a =>
        // We're already focused on "this" element
        val getA: List[A] => A = _ => a
        def modA(mod: A => A): List[A] => List[A] =
          list => list.modFirstWhere(thisA => eqBy(thisA) === eqBy(a), mod)

        viewList.zoom[A](getA)(modA)
      }

    def toSortedAgGridData[B, C](eqBy: A => B, sortBy: A => C)(implicit
      eq:                              Eq[B],
      ord:                             Ordering[C]
    ): List[ViewF[F, A]] =
      viewList.get
        .map { a =>
          // We're already focused on "this" element
          val getA: List[A] => A = _ => a
          def modA(mod: A => A): List[A] => List[A] =
            list => list.modFirstWhere(thisA => eqBy(thisA) === eqBy(a), mod).sortBy(sortBy)

          viewList.zoom[A](getA)(modA)
        }
        .sortBy(va => sortBy(va.get))
  }
}

object implicits extends ShorthandTypes with ListImplicits {
  implicit def appContext2ContextShift[F[_]](implicit ctx: AppContext[F]): ContextShift[F] = ctx.cs
  implicit def appContext2Timer[F[_]](implicit ctx:        AppContext[F]): Timer[F]        = ctx.timer
  implicit def appContext2Logger[F[_]](implicit ctx:       AppContext[F]): Logger[F]       = ctx.logger
  implicit def appContext2ExploreDBClient[F[_]](implicit
    ctx:                                                   AppContext[F]
  ): GraphQLWebSocketClient[F, ExploreDB] =
    ctx.clients.exploreDB
  implicit def appContext2ODBClient[F[_]](implicit
    ctx: AppContext[F]
  ): GraphQLWebSocketClient[F, ObservationDB] =
    ctx.clients.odb

  implicit class CoulombViewOps[F[_], N, U](val self: ViewF[F, Quantity[N, U]]) extends AnyVal {
    def stripQuantity: ViewF[F, N] = self.as(coulombIso[N, U])
  }

  implicit class CoulombViewOptOps[F[_], N, U](val self: ViewOptF[F, Quantity[N, U]])
      extends AnyVal {
    def stripQuantity: ViewOptF[F, N] = self.as(coulombIso[N, U])
  }

  implicit class RootModelOps(val rootModel: RootModel) extends AnyVal {
    def url[F[_]](implicit ctx: AppContext[F]): String =
      ctx.pageUrl(rootModel.tabs.focus, rootModel.focused)
  }
}
