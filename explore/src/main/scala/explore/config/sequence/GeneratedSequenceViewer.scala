// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import crystal.Pot
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import explore.*
import explore.model.AppContext
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.GeneratedSequenceSQL._
import queries.common.ObsQueriesGQL
import react.common.ReactFnProps

case class GeneratedSequenceViewer(obsId: Observation.Id, changed: View[Pot[Unit]])
    extends ReactFnProps(GeneratedSequenceViewer.component)

object GeneratedSequenceViewer:
  private type Props = GeneratedSequenceViewer

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy { (props, ctx) =>
        import ctx.given

        SequenceSteps
          .query(props.obsId)
          .map(_.observation.map(_.execution.config))
          .attemptPot
          .resetOnResourceSignals(
            ObsQueriesGQL.ObservationEditSubscription.subscribe[IO](props.obsId)
          )
      }
      .useEffectWithDepsBy((_, _, config) => config.toPot.void)((props, _, _) =>
        changedPot => props.changed.set(changedPot)
      )
      .render((props, _, config) =>
        props.changed.get
          .flatMap(_ => config.toPot.flatten)
          .render(
            _.fold[VdomNode](<.div("Default observation not found"))(config =>
              GeneratedSequenceTables(props.obsId, config)
            )
          )
      )
