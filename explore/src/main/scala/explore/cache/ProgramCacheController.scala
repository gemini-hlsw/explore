// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import clue.ErrorPolicy
import clue.StreamingClient
import clue.data.syntax.*
import crystal.Pot
import crystal.syntax.*
import explore.DefaultErrorPolicy
import explore.givens.given
import explore.model.Attachment
import explore.model.ConfigurationRequestWithObsIds
import explore.model.Execution
import explore.model.Group
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ProgramDetails
import explore.model.ProgramInfo
import explore.model.ProgramSummaries
import explore.model.ProgramTimeRange
import explore.utils.*
import fs2.Pipe
import fs2.Stream
import fs2.concurrent.Channel
import japgolly.scalajs.react.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.ConfigurationRequestEditInput
import lucuma.schemas.ObservationDB.Types.WhereObservation
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL
import queries.common.ProgramQueriesGQL
import queries.common.ProgramSummaryQueriesGQL
import queries.common.TargetQueriesGQL

import scala.concurrent.duration.*

case class ProgramCacheController(
  programId:           Program.Id,
  modProgramSummaries: (Pot[ProgramSummaries] => Pot[ProgramSummaries]) => IO[Unit]
)(using client: StreamingClient[IO, ObservationDB], logger: Logger[IO])
// Do not remove the explicit type parameter below, it confuses the compiler.
    extends ReactFnProps[ProgramCacheController](ProgramCacheController.component)
    with CacheControllerComponent.Props[ProgramSummaries]:
  val modState                             = modProgramSummaries
  given StreamingClient[IO, ObservationDB] = client
  given Logger[IO]                         = logger

object ProgramCacheController
    extends CacheControllerComponent[ProgramSummaries, ProgramCacheController]
    with CacheModifierUpdaters:

  private def drain[A, Id, R](
    fetch:      Option[Id] => IO[R],
    getList:    R => List[A],
    getHasMore: R => Boolean,
    getId:      A => Id
  ): IO[List[A]] = {
    def go(id: Option[Id], accum: List[A]): IO[List[A]] =
      fetch(id).flatMap(result =>
        val list = getList(result)
        if (getHasMore(result)) go(list.lastOption.map(getId), list)
        // Fetching with offset includes the offset, so .dropRight(1) ensures we don't include it twice.
        else (accum.dropRight(1) ++ list).pure[IO]
      )

    go(none, List.empty)
  }

  private def updateProgramTimes(
    programId: Program.Id
  )(using StreamingClient[IO, ObservationDB]): IO[ProgramSummaries => ProgramSummaries] =
    ProgramSummaryQueriesGQL
      .ProgramTimesQuery[IO]
      .query(programId)
      .map:
        _.program.fold(identity[ProgramSummaries]): times =>
          ProgramSummaries.programTimesPot.replace(Pot(times))

  private def updateObservationExecution(
    obsId: Observation.Id
  )(using StreamingClient[IO, ObservationDB]): IO[ProgramSummaries => ProgramSummaries] =
    ProgramSummaryQueriesGQL
      .ObservationExecutionQuery[IO]
      .query(obsId)(ErrorPolicy.IgnoreOnData)
      .map:
        _.observation
          .map(_.execution)
          .fold(identity[ProgramSummaries]): execution =>
            ProgramSummaries.obsExecutionPots.modify(_.updated(obsId, Pot(execution)))

  private def updateGroupTimeRange(groupId: Group.Id)(using
    StreamingClient[IO, ObservationDB]
  ): IO[ProgramSummaries => ProgramSummaries] =
    ProgramSummaryQueriesGQL
      .GroupTimeRangeQuery[IO]
      .query(groupId)
      .map:
        _.group
          .map(_.timeEstimateRange)
          .fold(identity[ProgramSummaries]): timeRange =>
            ProgramSummaries.groupTimeRangePots.modify(_.updated(groupId, Pot(timeRange)))

  override protected val initial: ProgramCacheController => IO[
    (ProgramSummaries, Stream[IO, ProgramSummaries => ProgramSummaries])
  ] = { props =>
    import props.given

    val optProgramDetails: IO[Option[ProgramDetails]] =
      ProgramSummaryQueriesGQL
        .ProgramDetailsQuery[IO]
        .query(props.programId)
        .map(_.program)
        .logTime("ProgramDetailsQuery")

    val targets: IO[List[TargetWithId]] =
      drain[TargetWithId, Target.Id, ProgramSummaryQueriesGQL.AllProgramTargets.Data](
        offset =>
          ProgramSummaryQueriesGQL
            .AllProgramTargets[IO]
            .query(props.programId.toWhereTarget, offset.orUnassign),
        _.targets.matches,
        _.targets.hasMore,
        _.id
      ).logTime("AllProgramTargets")

    val observations: IO[List[Observation]] =
      drain[Observation, Observation.Id, ProgramSummaryQueriesGQL.AllProgramObservations.Data](
        offset =>
          ProgramSummaryQueriesGQL
            .AllProgramObservations[IO]
            .query(props.programId.toWhereObservation, offset.orUnassign)(using
              // We need this because we currently get errors for things like having no targets
              ErrorPolicy.IgnoreOnData
            ),
        _.observations.matches,
        _.observations.hasMore,
        _.id
      ).logTime("AllProgramObservations")

    val configurationRequests: IO[List[ConfigurationRequestWithObsIds]] =
      drain[ConfigurationRequestWithObsIds,
            ConfigurationRequest.Id,
            ProgramSummaryQueriesGQL.AllProgramConfigurationRequests.Data
      ](
        offset =>
          ProgramSummaryQueriesGQL
            .AllProgramConfigurationRequests[IO]
            .query(props.programId, offset.orUnassign),
        _.program.foldMap(_.configurationRequests.matches),
        _.program.fold(false)(_.configurationRequests.hasMore),
        _.id
      ).logTime("AllProgramConfigurationRequests")

    val groups: IO[List[Group]] =
      ProgramQueriesGQL
        .ProgramGroupsQuery[IO]
        .query(props.programId)
        .map(_.program.toList.flatMap(_.allGroupElements.map(_.group).flattenOption))
        .logTime("ProgramGroupsQuery")

    val attachments: IO[List[Attachment]] =
      ProgramSummaryQueriesGQL
        .AllProgramAttachments[IO]
        .query(props.programId)
        .map:
          _.program.fold(List.empty)(_.attachments)
        .logTime("AllProgramAttachments")

    val programs: IO[List[ProgramInfo]] =
      drain[ProgramInfo, Program.Id, ProgramSummaryQueriesGQL.AllPrograms.Data](
        offset =>
          ProgramSummaryQueriesGQL
            .AllPrograms[IO]
            .query(offset.orUnassign),
        _.programs.matches,
        _.programs.hasMore,
        _.id
      )
        .logTime("AllPrograms")

    def initializeSummaries(
      observations: List[Observation],
      groups:       List[Group]
    ): IO[ProgramSummaries] =
      val obsPots: Map[Observation.Id, Pot[Execution]]            =
        observations.map(o => (o.id, pending)).toMap
      val groupPots: Map[Group.Id, Pot[Option[ProgramTimeRange]]] =
        groups.map(g => g.id -> pending).toMap
      (optProgramDetails, targets, attachments, programs, configurationRequests).mapN:
        case (pd, ts, as, ps, crs) =>
          ProgramSummaries
            .fromLists(
              pd,
              ts,
              observations,
              groups,
              as,
              ps,
              pending,
              obsPots,
              groupPots,
              crs
            )

    def combineTimesUpdates(
      observations: List[Observation],
      groups:       List[Group]
    ): Stream[IO, ProgramSummaries => ProgramSummaries] =
      // We want to update all the observations first, followed by the groups,
      // and then the program, because the program requires all of the observation times be calculated.
      // So, if we do it first, it could take a long time.
      Stream.emits(observations.map(_.id)).evalMap(updateObservationExecution) ++
        Stream.emits(groups.map(_.id)).evalMap(updateGroupTimeRange) ++
        Stream.eval(updateProgramTimes(props.programId))

    for
      (obs, groups) <- (observations, groups).parTupled
      summaries     <- initializeSummaries(obs, groups)
    yield (summaries, combineTimesUpdates(obs, groups))
  }

  override protected val updateStream
    : ProgramCacheController => Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] = {
    props =>
      // programUpdateChannel is just a sigal queue to trigger a (switchable) program times update.
      Resource.make(Channel.unbounded[IO, Unit])(_.close.void).flatMap { programUpdateChannel =>
        try {
          import props.given

          // Signal a program times update.
          val queryProgramTimes: IO[Unit] =
            programUpdateChannel.send(()).void

          val updateProgramTimesStream: Stream[IO, ProgramSummaries => ProgramSummaries] =
            programUpdateChannel.stream
              .switchMap(_ => Stream.eval(updateProgramTimes(props.programId)))

          def updateObservationsWorkflows(
            whereObservation: WhereObservation
          )(using StreamingClient[IO, ObservationDB]): IO[ProgramSummaries => ProgramSummaries] =
            ProgramSummaryQueriesGQL
              .ObservationsWorkflowQuery[IO]
              .query(whereObservation)
              .map:
                _.observations.matches
                  .map: m =>
                    ProgramSummaries.observations
                      .modify:
                        _.updatedWith(m.id)(_.map(Observation.workflow.replace(m.workflow)))
                  .combineAll

          // Changing the proposal's CfP can change the validations for observations,
          // eg: coordinates bounds, so we requery them all.
          // https://app.shortcut.com/lucuma/story/4412/update-warnings-without-page-reload
          val allObservationsValidationsUpdate: Pipe[
            IO,
            ProgramQueriesGQL.ProgramEditDetailsSubscription.Data,
            ProgramSummaries => ProgramSummaries
          ] =
            _.map(_.programEdit.value.proposal.flatMap(_.callId)).changes.void
              .evalMap(_ => updateObservationsWorkflows(props.programId.toWhereObservation))

          val updateProgramDetails: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
            ProgramQueriesGQL.ProgramEditDetailsSubscription
              .subscribe[IO](props.programId.toProgramEditInput)
              .map:
                _.broadcastThrough(
                  _.map: data => // Replace program.
                    ProgramSummaries.optProgramDetails.replace(data.programEdit.value.some),
                  allObservationsValidationsUpdate
                )

          val updateTargets: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
            TargetQueriesGQL.ProgramTargetsDelta
              .subscribe[IO](props.programId.toTargetEditInput)
              .map(_.map(data => modifyTargets(data.targetEdit)))

          val onlyExistingObs: Pipe[
            IO,
            ObsQueriesGQL.ProgramObservationsDelta.Data,
            ObsQueriesGQL.ProgramObservationsDelta.Data
          ] =
            _.filter(_.observationEdit.meta.exists(_.existence === Existence.Present))

          val onlyExistingGroups: Pipe[
            IO,
            ProgramQueriesGQL.GroupEditSubscription.Data,
            ProgramQueriesGQL.GroupEditSubscription.Data
          ] =
            _.filter(_.groupEdit.meta.exists(_.existence === Existence.Present))

          val obsTimesUpdates: Pipe[
            IO,
            ObsQueriesGQL.ProgramObservationsDelta.Data,
            ProgramSummaries => ProgramSummaries
          ] = keyedSwitchEvalMap(
            _.observationEdit.observationId,
            data =>
              updateObservationExecution(data.observationEdit.observationId) <* queryProgramTimes
          )

          extension (data: ProgramQueriesGQL.ConfigurationRequestSubscription.Data)
            private def obsIdList: List[Observation.Id] =
              data.configurationRequestEdit.configurationRequest.toList
                .flatMap(_.applicableObservations)

          // We'll request updates to all the workflows at once. Since this
          // is being updated due to the configuration request change, the workflows
          // "shouldn't" be expensive and we're unlikely to need to cancel
          // unless the same configuration request is edited so using the set
          // of ids as a key should be OK.
          val obsWorkflowUpdates: Pipe[
            IO,
            ProgramQueriesGQL.ConfigurationRequestSubscription.Data,
            ProgramSummaries => ProgramSummaries
          ] = keyedSwitchEvalMap(
            _.obsIdList.toSet,
            _.obsIdList match
              case Nil             => IO.pure(identity)
              case nonEmptyObsList =>
                updateObservationsWorkflows(nonEmptyObsList.toWhereObservation)
          )

          val groupTimeRangeUpdate: Pipe[
            IO,
            ProgramQueriesGQL.GroupEditSubscription.Data,
            ProgramSummaries => ProgramSummaries
          ] = keyedSwitchEvalMap(
            _.groupEdit.value.map(_.id),
            _.groupEdit.value
              .map: group =>
                updateGroupTimeRange(group.id) <* queryProgramTimes
              .getOrElse(IO(identity))
          )

          val updateObservations: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
            ObsQueriesGQL.ProgramObservationsDelta
              .subscribe[IO](props.programId.toObservationEditInput)(using
                summon,
                ErrorPolicy.IgnoreOnData
              )
              .map:
                _.broadcastThrough(
                  _.map(data => modifyObservations(data.observationEdit)),
                  onlyExistingObs.andThen(obsTimesUpdates)
                )

          val updateGroups: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
            ProgramQueriesGQL.GroupEditSubscription
              .subscribe[IO](props.programId.toProgramEditInput)
              .map:
                _.broadcastThrough(
                  _.map(data => modifyGroups(data.groupEdit)),
                  onlyExistingGroups.andThen(groupTimeRangeUpdate)
                )

          val updateConfigurationRequests
            : Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
            ProgramQueriesGQL.ConfigurationRequestSubscription
              .subscribe[IO](ConfigurationRequestEditInput(props.programId.assign))
              .map:
                _.broadcastThrough(
                  _.map(data => modifyConfigurationRequests(data.configurationRequestEdit)),
                  obsWorkflowUpdates
                )

          // Right now the programEdit subsription isn't fine grained enough to
          // differentiate what got updated, so we alway update all the attachments.
          // Hopefully this will change in the future.
          val updateAttachments: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
            ProgramQueriesGQL.ProgramEditAttachmentSubscription
              .subscribe[IO](props.programId.toProgramEditInput)
              .map(_.map(data => modifyAttachments(data.programEdit)))

          val updatePrograms: Resource[IO, Stream[IO, ProgramSummaries => ProgramSummaries]] =
            ProgramQueriesGQL.ProgramInfoDelta
              .subscribe[IO]()
              .map:
                _.map(data => modifyPrograms(data.programEdit))

          // TODO Handle errors, disable transparent resubscription upon connection loss.
          List(
            updateProgramDetails,
            updateTargets,
            updateObservations,
            updateGroups,
            updateAttachments,
            updatePrograms,
            updateConfigurationRequests
          ).sequence.map: updateStreams =>
            (updateStreams :+ updateProgramTimesStream)
              .reduceLeft(_.merge(_))
              .reduceWithin(150.millis, _ andThen _) // Group updates within 150ms
        } catch {
          case t: Throwable =>
            println("ERROR INITIALIZING ProgramCache!")
            t.printStackTrace()
            throw t
        }
      }
  }
