// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObserveClass
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message
import lucuma.schemas.odb.SequenceSQL.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL

case class GeneratedSequenceViewer(
  programId:       Program.Id,
  obsId:           Observation.Id,
  targetIds:       List[Target.Id],
  snPerClass:      Map[ObserveClass, SignalToNoise],
  sequenceChanged: View[Pot[Unit]]
) extends ReactFnProps(GeneratedSequenceViewer.component)

object GeneratedSequenceViewer:
  private type Props = GeneratedSequenceViewer

  private given Reusability[InstrumentExecutionConfig] = Reusability.byEq

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy: (props, ctx) =>
        import ctx.given

        val targetChanges = props.targetIds.traverse: targetId =>
          TargetQueriesGQL.TargetEditSubscription
            .subscribe[IO](targetId)

        SequenceQuery[IO]
          .query(props.obsId)
          .map(_.observation.flatMap(_.execution.config))
          .attemptPot
          .resetOnResourceSignals(
            for {
              o <- ObsQueriesGQL.ObservationEditSubscription.subscribe[IO](props.obsId)
              t <- targetChanges
            } yield o.merge(t.sequence)
          )
      .useEffectWithDepsBy((_, _, config) => config.toPot.flatten): (props, _, _) =>
        changedPot => props.sequenceChanged.set(changedPot.void)
      .render: (props, _, config) =>
        props.sequenceChanged.get
          .flatMap(_ => config.toPot.flatten)
          .renderPot(
            _.fold[VdomNode](<.div("Default observation not found")) {
              case InstrumentExecutionConfig.GmosNorth(config) =>
                GmosNorthGeneratedSequenceTables(props.obsId, config, props.snPerClass)
              case InstrumentExecutionConfig.GmosSouth(config) =>
                GmosSouthGeneratedSequenceTables(props.obsId, config, props.snPerClass)
            },
            errorRender = m => {
              val msg = m match {
                case clue.ResponseException(errors, _)
                    if errors.exists(_.message.startsWith("ITC returned")) =>
                  "Cannot calculate ITC, please check your configuration"
                case _ =>
                  "No sequence available, you may need to setup a configuration"
              }
              <.div(
                ExploreStyles.SequencesPanelError,
                Message(text = msg,
                        severity = Message.Severity.Warning,
                        icon = Icons.ExclamationTriangle
                )
              )
            }
          )
