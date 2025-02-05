// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import clue.StreamingClient
import clue.data.syntax.*
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
import lucuma.schemas.ObservationDB.Types.ExecutionEventAddedInput
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.odb.SequenceQueriesGQL.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL
import queries.common.VisitQueriesGQL
import queries.common.VisitQueriesGQL.*

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
  ) extends ReactFnProps(Body.component)

  private object Body:
    private case class SequenceData(
      config:     InstrumentExecutionConfig,
      snPerClass: Map[ObserveClass, SignalToNoise]
    ) derives Eq

    private object SequenceData:
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

    def obsEditSubscription(obsId: Observation.Id)(using StreamingClient[IO, ObservationDB]) =
      ObsQueriesGQL.ObservationEditSubscription
        .subscribe[IO]:
          obsId.toObservationEditInput

    def executionEventAddedSubscription(obsId: Observation.Id)(using
      StreamingClient[IO, ObservationDB]
    ) = VisitQueriesGQL.ExecutionEventAddedSubscription.subscribe[IO](
      ExecutionEventAddedInput(observationId = obsId.assign)
    )

    def queryTriggers(obsId: Observation.Id)(using StreamingClient[IO, ObservationDB]) =
      for {
        o <- obsEditSubscription(obsId)
        e <- executionEventAddedSubscription(obsId)
      } yield o.merge(e)

    private val component =
      ScalaFnComponent[Body]: props =>
        for {
          ctx          <- useContext(AppContext.ctx)
          visits       <- useStreamResourceOnMount: // Query Visits
                            import ctx.given

                            ObservationVisits[IO]
                              .query(props.obsId)
                              .raiseGraphQLErrors
                              .map(_.observation.flatMap(_.execution))
                              .attemptPot
                              .reRunOnResourceSignals:
                                queryTriggers(props.obsId)
          sequenceData <- useStreamResourceOnMount: // Query Sequence
                            import ctx.given

                            // sequence also triggers on target changes
                            val targetChanges = props.targetIds.map: targetId =>
                              TargetQueriesGQL.TargetEditSubscription
                                .subscribe[IO](targetId)

                            SequenceQuery[IO]
                              .query(props.obsId)
                              .raiseGraphQLErrors
                              .map(SequenceData.fromOdbResponse)
                              .attemptPot
                              .reRunOnResourceSignals(queryTriggers(props.obsId), targetChanges*)
          _            <- useEffectWithDeps((visits.toPot.flatten, sequenceData.toPot.flatten).tupled):
                            dataPot => props.sequenceChanged.set(dataPot.void)
        } yield props.sequenceChanged.get
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
