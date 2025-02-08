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
import lucuma.core.enums.StepStage

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

  // TODO Move this to some util package
  import cats.effect.Async
  import cats.effect.Sync
  import cats.effect.Resource
  import fs2.concurrent.Topic
  import cats.effect.syntax.all.*
  extension [F[_]: Async, A](stream: fs2.Stream[F, A])
    /**
     * Given a `Stream`, starts it in the background and allows the creation of multiple other
     * `Streams` that mirror its output. Useful when we want multiple `Streams` to consume the same
     * output.
     *
     * When we start the same stream multiple times, we have no guarantee that all the fibers will
     * read all the elemnts. In particular, if the stream is backed by a queue, the fibers will
     * steal elements from the queue, and each element will only make it to one of the fibers.
     */
    def startSubscriber: Resource[F, Resource[F, fs2.Stream[F, A]]] =
      for
        t <- Resource.make(Topic[F, A])(_.close.void)
        _ <- stream.through(t.publish).compile.drain.background
      yield Resource.suspend(Sync[F].delay(t.subscribeAwaitUnbounded))

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
                .ignoreGraphQLErrors
                .map(_.void)
                .flatMap(startSubscriber)
          targetEditSignal                        <-
            useResourceOnMount: // Subscribe to target edits
              props.targetIds
                .traverse: targetId =>
                  TargetQueriesGQL.TargetEditSubscription
                    .subscribe[IO](targetId)
                    .ignoreGraphQLErrors
                .map(_.combineAll.void)
                .flatMap(startSubscriber)
          stepExecutedSignal                      <-
            useResourceOnMount: // Subscribe to step executed events
              VisitQueriesGQL.StepSubscription
                .subscribe[IO](props.obsId)
                .ignoreGraphQLErrors
                .map(_.filter(_.executionEventAdded.value.stepStage === StepStage.EndStep).void)
                .flatMap(startSubscriber)
          visits                                  <-
            useStreamResourceWhenDepsReady((obsEditSignal, stepExecutedSignal).tupled):
              (obsEditSignal, stepExecutedSignal) => // Query Visits
                ObservationVisits[IO]
                  .query(props.obsId)
                  .raiseGraphQLErrors
                  .map(_.observation.flatMap(_.execution))
                  .attemptPot
                  .reRunOnResourceSignals:
                    NonEmptyList.of(obsEditSignal, stepExecutedSignal)
          sequenceData                            <-
            useStreamResourceWhenDepsReady(
              (obsEditSignal, targetEditSignal, stepExecutedSignal).tupled
            ): (obsEditSignal, targetEditSignal, stepExecutedSignal) => // Query Sequence
              SequenceQuery[IO]
                .query(props.obsId)
                .raiseGraphQLErrors
                .map(SequenceData.fromOdbResponse)
                .attemptPot
                .reRunOnResourceSignals:
                  NonEmptyList.of(obsEditSignal, targetEditSignal, stepExecutedSignal)
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
