// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import cats.syntax.all.*
import clue.ErrorPolicy
import crystal.Pot
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.reusability.given
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
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.odb.SequenceSQL.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL
import queries.common.VisitQueriesGQL.*

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
      .useStreamResourceOnMountBy: (props, ctx) => // Query Visits
        import ctx.given

        ObservationVisits[IO]
          .query(props.obsId)(ErrorPolicy.IgnoreOnData)
          .map(_.flatMap(_.observation.flatMap(_.execution)))
          .attemptPot
          .resetOnResourceSignals:
            ObsQueriesGQL.ObservationEditSubscription
              .subscribe[IO]:
                props.obsId.toObservationEditInput
      .useStreamResourceOnMountBy: (props, ctx, _) => // Query Sequence
        import ctx.given

        val targetChanges = props.targetIds.traverse: targetId =>
          TargetQueriesGQL.TargetEditSubscription
            .subscribe[IO](targetId)

        SequenceQuery[IO]
          .query(props.obsId)
          .map(_.observation.flatMap(_.execution.config))
          .attemptPot
          .resetOnResourceSignals:
            for
              o <- ObsQueriesGQL.ObservationEditSubscription
                     .subscribe[IO](props.obsId.toObservationEditInput)
              t <- targetChanges
            yield o.merge(t.sequence)
      .useEffectWithDepsBy((_, _, visits, config) =>
        (visits.toPot.flatten, config.toPot.flatten).tupled
      ): (props, _, _, _) =>
        dataPot => props.sequenceChanged.set(dataPot.void)
      .render: (props, _, visits, config) =>
        props.sequenceChanged.get
          .flatMap(_ => (visits.toPot.flatten, config.toPot.flatten).tupled) // tupled for Pot
          .renderPot( 
            _.tupled // tupled for Option
              .fold[VdomNode](<.div("Empty or incomplete sequence data returned by server")) {
                case (
                      ExecutionVisits.GmosNorth(_, visits),
                      InstrumentExecutionConfig.GmosNorth(config)
                    ) =>
                      GmosNorthSequenceTable(visits, config, props.snPerClass)
                case (
                      ExecutionVisits.GmosSouth(_, visits),
                      InstrumentExecutionConfig.GmosSouth(config)
                    ) =>
                      GmosSouthSequenceTable(visits, config, props.snPerClass)
                case _ => <.div("MISMATCH!!!") // TODO Nice error message, which should never happen BTW
              },
            errorRender = m =>
              val msg = m match
                case clue.ResponseException(errors, _)
                    if errors.exists(_.message.startsWith("ITC returned")) =>
                  "Cannot calculate ITC, please check your configuration"
                case _ =>
                  "No sequence available, you may need to setup a configuration"

              <.div(
                ExploreStyles.SequencesPanelError,
                Message(
                  text = msg,
                  severity = Message.Severity.Warning,
                  icon = Icons.ExclamationTriangle
                )
              )
          )
