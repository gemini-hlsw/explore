// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.hooks._
import explore.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.sequence._
import queries.common.GeneratedSequenceSQL._
import react.common._
import queries.common.ObsQueriesGQL
import cats.effect.IO

final case class GeneratedSequenceViewer(obsId: Observation.Id)(implicit val ctx: AppContextIO)
    extends ReactFnProps[GeneratedSequenceViewer](GeneratedSequenceViewer.component)

object GeneratedSequenceViewer {
  type Props = GeneratedSequenceViewer

  private def renderFn(config: Option[FutureExecutionConfig]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(GeneratedSequenceTables.apply)

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStreamResourceOnMountBy { props =>
        implicit val ctx = props.ctx

        SequenceSteps
          .query(props.obsId)
          .map(_.observation.map(_.execution.config))
          .attemptPot
          .resetOnResourceSignals(
            ObsQueriesGQL.ObservationEditSubscription.subscribe[IO](props.obsId)
          )
      }
      .render((_, config) => potRender(renderFn)(config.flatten))
}
