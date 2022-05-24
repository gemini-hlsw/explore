// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.effect.Resource
import crystal.react.StreamResourceRenderer
import crystal.react.reuse._
import explore.AppCtx
import explore.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.sequence._
import lucuma.ui.reusability._
import queries.common.GeneratedSequenceSQL._
import react.common._

final case class GeneratedSequenceViewer(obsId: Observation.Id)
    extends ReactFnProps[GeneratedSequenceViewer](GeneratedSequenceViewer.component)

object GeneratedSequenceViewer {
  type Props = GeneratedSequenceViewer

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  private def renderFn(config: Option[FutureExecutionConfig]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(GeneratedSequenceTables.apply)

  val component =
    ScalaFnComponent
      .withReuse[Props](props =>
        AppCtx.using { implicit ctx =>
          StreamResourceRenderer[Option[FutureExecutionConfig]](
            Resource.eval(
              IO(
                fs2.Stream.eval(
                  SequenceSteps
                    .query(props.obsId)
                    .map(_.observation.flatMap(_.execution.config))
                )
              )
            ),
            potRenderWithReuse((renderFn _).reuseAlways)
          )
        }
      )
}
