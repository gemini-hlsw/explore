// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.Resource
import cats.effect.Sync
import crystal.react.ReuseView
import crystal.react.StreamResourceRendererMod
import crystal.react.reuse._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.util.DefaultEffects.{ Async => DefaultA }
import japgolly.scalajs.react.util.DefaultEffects.{ Sync => DefaultS }
import japgolly.scalajs.react.util.Effect.UnsafeSync
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.common.ReactFnProps

import scala.concurrent.duration._
import scala.reflect.ClassTag

final case class LiveQuery[A](resource: Resource[DefaultA, fs2.Stream[DefaultA, A]])(
  val render:                           ReuseView[A] ==> VdomNode
)(implicit
  val reuse:                            Reusability[A],
  val classTag:                         ClassTag[A],
  val DefaultS:                         Sync[DefaultS],
  val dispatch:                         UnsafeSync[DefaultS],
  val logger:                           Logger[DefaultA]
) extends ReactFnProps[LiveQuery[Any]](LiveQuery.component)

object LiveQuery {
  type Props[A] = LiveQuery[A]

  private def componentBuilder[A] =
    ScalaFnComponent[Props[A]] { props =>
      import props.reuse
      import props.classTag
      import props.DefaultS
      import props.dispatch
      import props.logger

      StreamResourceRendererMod(
        props.resource,
        potRenderWithReuse(props.render)
      ).withHoldAfterMod(2.seconds)
    }

  val component = componentBuilder[Any]
}
