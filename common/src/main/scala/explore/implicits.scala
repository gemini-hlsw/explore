// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.annotation.unused

import cats._
import cats.effect.ContextShift
import cats.effect.Timer
import cats.syntax.all._
import explore.model.AppContext
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
}

object implicits extends ShorthandTypes with ListImplicits {
  implicit def appContext2ContextShift[F[_]](implicit ctx: AppContext[F]): ContextShift[F] = ctx.cs
  implicit def appContext2Timer[F[_]](implicit ctx:        AppContext[F]): Timer[F]        = ctx.timer
  implicit def appContext2Logger[F[_]](implicit ctx:       AppContext[F]): Logger[F]       = ctx.logger
}
