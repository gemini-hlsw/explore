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
import explore.DefaultErrorPolicy
import explore.model.GroupElement
import explore.model.ObsAttachment
import explore.model.ObsSummary
import explore.model.ObservationExecutionMap
import explore.model.ProgramDetails
import explore.model.ProgramInfo
import explore.model.ProgramSummaries
import explore.model.ProposalAttachment
import explore.utils.keyedSwitchEvalMap
import explore.utils.reduceWithin
import japgolly.scalajs.react.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.input.*
import lucuma.ui.reusability.given
import queries.common.ObsQueriesGQL
import queries.common.ProgramQueriesGQL
import queries.common.ProgramSummaryQueriesGQL
import queries.common.TargetQueriesGQL

import scala.concurrent.duration.*

case class ProgramCache(
  programId:           Program.Id,
  roleId:              Option[String], // Just to refresh if the role has changed
  setProgramSummaries: Option[ProgramSummaries] => IO[Unit]
)(using client: StreamingClient[IO, ObservationDB])
    extends ReactFnProps[ProgramCache](ProgramCache.component)
    with CacheComponent.Props[ProgramSummaries]:
  val setState                             = setProgramSummaries
  given StreamingClient[IO, ObservationDB] = client

object ProgramCache
    extends CacheComponent[ProgramSummaries, ProgramCache]
    with CacheModifierUpdaters:
  given Reusability[ProgramCache] = Reusability.by(p => (p.programId, p.roleId))

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
        _.program.fold(identity[ProgramSummaries])(times =>
          ProgramSummaries.programTimesPot.replace(Pot(times))
        )

  private def updateObservationExecution(
    obsId: Observation.Id
  )(using StreamingClient[IO, ObservationDB]): IO[ProgramSummaries => ProgramSummaries] =
    ProgramSummaryQueriesGQL
      .ObservationExecutionQuery[IO]
      .query(obsId)(ErrorPolicy.IgnoreOnData)
      .map:
        _.observation
          .map(_.execution)
          .fold(identity[ProgramSummaries])(execution =>
            ProgramSummaries.obsExecutionPots.modify(_.updated(obsId, Pot(execution)))
          )

  override protected val initial: ProgramCache => IO[
    (ProgramSummaries, fs2.Stream[IO, ProgramSummaries => ProgramSummaries])
  ] = { props =>
    import props.given

    val optProgramDetails: IO[Option[ProgramDetails]] =
      ProgramSummaryQueriesGQL
        .ProgramDetailsQuery[IO]
        .query(props.programId)
        .map(_.program)

    val targets: IO[List[TargetWithId]] =
      drain[TargetWithId, Target.Id, ProgramSummaryQueriesGQL.AllProgramTargets.Data](
        offset =>
          ProgramSummaryQueriesGQL
            .AllProgramTargets[IO]
            .query(props.programId.toWhereTarget, offset.orUnassign),
        _.targets.matches,
        _.targets.hasMore,
        _.id
      )

    val observations: IO[List[ObsSummary]] =
      drain[ObsSummary, Observation.Id, ProgramSummaryQueriesGQL.AllProgramObservations.Data](
        offset =>
          ProgramSummaryQueriesGQL
            .AllProgramObservations[IO]
            .query(props.programId.toWhereObservation, offset.orUnassign),
        _.observations.matches,
        _.observations.hasMore,
        _.id
      )

    val groups: IO[List[GroupElement]] =
      ProgramQueriesGQL
        .ProgramGroupsQuery[IO]
        .query(props.programId)
        .map(_.program.toList.flatMap(_.allGroupElements))

    val attachments: IO[(List[ObsAttachment], List[ProposalAttachment])] =
      ProgramSummaryQueriesGQL
        .AllProgramAttachments[IO]
        .query(props.programId)
        .map(
          _.program.fold(List.empty, List.empty)(p => (p.obsAttachments, p.proposalAttachments))
        )

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

    def initializeSummaries(observations: List[ObsSummary]): IO[ProgramSummaries] =
      val obsPots = observations.map(o => (o.id, Pot.pending)).toMap
      (optProgramDetails, targets, groups, attachments, programs).mapN {
        case (pd, ts, gs, (oas, pas), ps) =>
          ProgramSummaries.fromLists(pd, ts, observations, gs, oas, pas, ps, Pot.pending, obsPots)
      }

    def combineTimesUpdates(
      observations: List[ObsSummary]
    ): fs2.Stream[IO, ProgramSummaries => ProgramSummaries] =
      // We want to update all the observations first, followed by the program, because
      // the program requires all of the observation times be calculated. So, if we do
      // it first, it could take a long time.
      fs2.Stream(observations.map(_.id)*).evalMap(updateObservationExecution) ++
        fs2.Stream.eval(updateProgramTimes(props.programId))

    for
      obs       <- observations
      summaries <- initializeSummaries(obs)
    yield (summaries, combineTimesUpdates(obs))
  }

  override protected val updateStream
    : ProgramCache => Resource[IO, fs2.Stream[IO, ProgramSummaries => ProgramSummaries]] = {
    props =>
      try {
        import props.given

        val updateProgramDetails =
          ProgramQueriesGQL.ProgramEditDetailsSubscription
            .subscribe[IO](props.programId.toProgramEditInput)
            .map:
              _.broadcastThrough(
                _.map: data => // Replace program and reset times.
                  ProgramSummaries.optProgramDetails.replace(data.programEdit.value.some) >>>
                    ProgramSummaries.programTimesPot.replace(Pot.pending),
                _.switchMap: data => // Query new time, cancel if there's a new update.
                  fs2.Stream.eval(updateProgramTimes(props.programId))
              )

        val updateTargets =
          TargetQueriesGQL.ProgramTargetsDelta
            .subscribe[IO](props.programId.toTargetEditInput)
            .map(_.map(data => modifyTargets(data.targetEdit)))

        val obsTimesUpdates = keyedSwitchEvalMap[
          IO,
          ObsQueriesGQL.ProgramObservationsDelta.Data,
          ProgramSummaries => ProgramSummaries,
          Observation.Id
        ](
          _.observationEdit.value.id,
          data =>
            updateObservationExecution(data.observationEdit.value.id).flatMap: updateObs =>
              updateProgramTimes(props.programId).map: updateProgram =>
                updateObs >>> updateProgram
        )

        val updateObservations =
          ObsQueriesGQL.ProgramObservationsDelta
            .subscribe[IO](props.programId.toObservationEditInput)(summon, ErrorPolicy.IgnoreOnData)
            .map:
              _.broadcastThrough(
                _.map(data => modifyObservations(data.observationEdit)),
                obsTimesUpdates
              )

        val updateGroups =
          ProgramQueriesGQL.GroupEditSubscription
            .subscribe[IO](props.programId.toProgramEditInput)
            .map(_.map(data => modifyGroups(data.groupEdit)))

        // Right now the programEdit subsription isn't fine grained enough to
        // differentiate what got updated, so we alway update all the attachments.
        // Hopefully this will change in the future.
        val updateAttachments =
          ProgramQueriesGQL.ProgramEditAttachmentSubscription
            .subscribe[IO](props.programId.toProgramEditInput)
            .map(_.map(data => modifyAttachments(data.programEdit)))

        val updatePrograms =
          ProgramQueriesGQL.ProgramInfoDelta
            .subscribe[IO]()
            .map(_.map(data => modifyPrograms(data.programEdit)))

        // TODO Handle errors, disable transparent resubscription upon connection loss.
        List(
          updateProgramDetails,
          updateTargets,
          updateObservations,
          updateGroups,
          updateAttachments,
          updatePrograms
        ).sequence.map:
          _.reduceLeft(_.merge(_))
            .reduceWithin(150.millis, _ andThen _) // Group updates within 150ms
      } catch {
        case t: Throwable =>
          println("ERROR INITIALIZING ProgramCache!")
          t.printStackTrace()
          throw t
      }
  }
