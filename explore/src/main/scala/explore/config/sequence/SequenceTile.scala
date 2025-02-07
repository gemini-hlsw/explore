// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import clue.StreamingClient
import crystal.Pot
import crystal.react.*
import crystal.react.View
import crystal.react.given
import crystal.react.hooks.*
import explore.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.AsterismIds
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObserveClass
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.odb.SequenceQueriesGQL.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL
import queries.common.VisitQueriesGQL
import queries.common.VisitQueriesGQL.*
import cats.data.NonEmptyList

object SequenceTile:
  def apply(
    programId:       Program.Id,
    obsId:           Observation.Id,
    obsExecution:    Pot[Execution],
    asterismIds:     AsterismIds,
    sequenceChanged: View[Pot[Unit]]
  ) =
    Tile(
      ObsTabTileIds.SequenceId.id,
      s"Sequence"
    )(
      _ =>
        Body(
          programId,
          obsId,
          asterismIds.toList,
          sequenceChanged
        ),
      (_, _) => Title(obsExecution)
    )

  private case class Body(
    programId:       Program.Id,
    obsId:           Observation.Id,
    targetIds:       List[Target.Id],
    sequenceChanged: View[Pot[Unit]]
  ) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>

        case class SequenceData(
          config:     InstrumentExecutionConfig,
          snPerClass: Map[ObserveClass, SignalToNoise]
        ) derives Eq

        object SequenceData:
          def fromOdbResponse(data: SequenceQuery.Data): Option[SequenceData] =
            data.observation.flatMap: obs =>
              obs.execution.config.map: config =>
                SequenceData(
                  config,
                  Map.empty
                  // (
                  //   ObserveClass.Science     -> obs.itc.science.selected.signalToNoise,
                  //   ObserveClass.Acquisition -> obs.itc.acquisition.selected.signalToNoise
                  // )
                )

          given Reusability[SequenceData] = Reusability.byEq
        end SequenceData

        for
          ctx                                     <- useContext(AppContext.ctx)
          given StreamingClient[IO, ObservationDB] = ctx.clients.odb
          obsEditSignal                           <-
            useResourceOnMount: // Subscribe to observation edits
              ObsQueriesGQL.ObservationEditSubscription
                .subscribe[IO](props.obsId.toObservationEditInput)
                .map(_.void)
          targetEditSignal                        <-
            useResourceOnMount: // Subscribe to target edits
              props.targetIds
                .traverse: targetId =>
                  TargetQueriesGQL.TargetEditSubscription
                    .subscribe[IO](targetId)
                .map(_.combineAll.void)
          stepExecutedSignal                      <-
            useResourceOnMount: // Subscribe to step executed events
              VisitQueriesGQL.StepEndedSubscription
                .subscribe[IO](props.obsId)
                .map(_.void)
          visits                                  <-
            // Nice to have: useStreamWhenDepsReady
            useStream((obsEditSignal, stepExecutedSignal).tupled.void): _ => // Query Visits
              (obsEditSignal, stepExecutedSignal).tupled
                .map: (oes, ses) =>
                  val visitsQuery: IO[Pot[Option[ExecutionVisits]]] =
                    ObservationVisits[IO]
                      .query(props.obsId)
                      .raiseGraphQLErrors
                      .map(_.observation.flatMap(_.execution))
                      .attemptPot

                  visitsQuery.reRunOnSignals(NonEmptyList.of(oes, ses))
                .toOption
                .getOrElse(fs2.Stream.empty)
          sequenceData                            <-
            // Nice to have: useStreamWhenDepsReady
            useStream((obsEditSignal, targetEditSignal, stepExecutedSignal).tupled.void):
              _ => // Query Sequence
                (obsEditSignal, targetEditSignal, stepExecutedSignal).tupled
                  .map: (oes, tes, ses) =>
                    val sequenceQuery: IO[Pot[Option[SequenceData]]] =
                      SequenceQuery[IO]
                        .query(props.obsId)
                        .raiseGraphQLErrors
                        .map(SequenceData.fromOdbResponse)
                        .attemptPot

                    sequenceQuery
                      .reRunOnSignals(NonEmptyList.of(oes, tes, ses))
                  .toOption
                  .getOrElse(fs2.Stream.empty)
          _                                       <-
            useEffectWithDeps((visits.toPot.flatten, sequenceData.toPot.flatten).tupled): dataPot =>
              props.sequenceChanged.set(dataPot.void)
        yield props.sequenceChanged.get
          .flatMap(_ => (visits.toPot.flatten, sequenceData.toPot.flatten).tupled) // tupled for Pot
          .renderPot(
            (visitsOpt, sequenceDataOpt) =>
              // TODO Show visits even if sequence data is not available
              sequenceDataOpt
                .fold[VdomNode](<.div("Empty or incomplete sequence data returned by server")) {
                  case SequenceData(InstrumentExecutionConfig.GmosNorth(config), snPerClass) =>
                    GmosNorthSequenceTable(
                      visitsOpt.collect { case ExecutionVisits.GmosNorth(visits) =>
                        visits.toList
                      }.orEmpty,
                      config,
                      snPerClass
                    )
                  case SequenceData(InstrumentExecutionConfig.GmosSouth(config), snPerClass) =>
                    GmosSouthSequenceTable(
                      visitsOpt.collect { case ExecutionVisits.GmosSouth(visits) =>
                        visits.toList
                      }.orEmpty,
                      config,
                      snPerClass
                    )
                },
            errorRender = m =>
              <.div(ExploreStyles.SequencesPanelError)(
                Message(
                  text = m.getMessage,
                  severity = Message.Severity.Warning,
                  icon = Icons.ExclamationTriangle
                )
              )
          )
      )

  private case class Title(obsExecution: Pot[Execution]) extends ReactFnProps(Title)

  private object Title
      extends ReactFnComponent[Title](props =>
        <.span(ExploreStyles.SequenceTileTitle)(
          props.obsExecution.orSpinner: execution =>
            val programTimeCharge = execution.programTimeCharge.value

            val executed = timeDisplay("Executed", programTimeCharge)

            execution.programTimeEstimate
              .map: plannedTime =>
                val total   = programTimeCharge +| plannedTime
                val pending = timeDisplay("Pending", plannedTime)
                val planned = timeDisplay("Planned", total)

                React.Fragment(
                  HelpIcon("target/main/sequence-times.md".refined),
                  planned,
                  executed,
                  pending
                )
              .getOrElse(executed)
        )
      )
