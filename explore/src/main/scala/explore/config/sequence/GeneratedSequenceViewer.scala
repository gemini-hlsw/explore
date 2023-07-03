// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import crystal.Pot
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import explore.*
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.schemas.odb.SequenceSQL.*
import queries.common.ObsQueriesGQL
import react.common.ReactFnProps

case class GeneratedSequenceViewer(
  programId: Program.Id,
  obsId:     Observation.Id,
  changed:   View[Pot[Unit]]
) extends ReactFnProps(GeneratedSequenceViewer.component)

object GeneratedSequenceViewer:
  private type Props = GeneratedSequenceViewer

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy { (props, ctx) =>
        import ctx.given

        SequenceQuery[IO]
          .query(props.obsId)
          .map(_.observation.flatMap(_.sequence.map(_.executionConfig)))
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
          .renderPot(
            _.fold[VdomNode](<.div("Default observation not found")) {
              case InstrumentExecutionConfig.GmosNorth(config) =>
                GmosNorthGeneratedSequenceTables(props.obsId, config)
              case InstrumentExecutionConfig.GmosSouth(config) =>
                GmosSouthGeneratedSequenceTables(props.obsId, config)
            }
          )
      )
