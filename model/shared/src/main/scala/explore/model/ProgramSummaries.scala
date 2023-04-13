// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.data.NonEmptySet
import cats.derived.*
import cats.implicits.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class ProgramSummaries(
  targets:      TargetList,
  observations: ObservationList
) derives Eq:
  lazy val asterismGroups: AsterismGroupList =
    SortedMap.from(
      observations.values
        .map(obs => obs.id -> obs.scienceTargetIds)
        .groupMap(_._2)(_._1)
        .map((targets, observations) =>
          ObsIdSet(NonEmptySet.of(observations.head, observations.tail.toList: _*)) -> SortedSet
            .from(targets)
        )
    )

  lazy val targetObservations: Map[Target.Id, SortedSet[Observation.Id]] =
    observations.values
      .flatMap(obs => obs.scienceTargetIds.map(targetId => targetId -> obs.id))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(obsIds => SortedSet.from(obsIds))
      .toMap

  // Might not be used after all
  lazy val targetsWithObs: TargetWithObsList =
    targets.map((targetId, target) =>
      targetId -> TargetWithObs(target, targetObservations.get(targetId).orEmpty)
    )

  lazy val constraintGroups: ConstraintGroupList =
    SortedMap.from(
      observations.values
        .groupMap(_.constraints)(_.id)
        .map((c, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList: _*) -> c)
    )

  def cloneObsWithTargets(
    originalId: Observation.Id,
    clonedId:   Observation.Id,
    targetIds:  List[Target.Id]
  ): Option[ObsSummary] =
    observations
      .getValue(originalId)
      .map(_.copy(id = clonedId, scienceTargetIds = SortedSet.from(targetIds)))

  def insertObs(obsSummary: ObsSummary): ProgramSummaries =
    ProgramSummaries.observations.modify(
      _.inserted(obsSummary.id, obsSummary, observations.length)
    )(this)

  def removeObs(obsId: Observation.Id): ProgramSummaries =
    ProgramSummaries.observations.modify(_.removed(obsId))(this)

object ProgramSummaries:
  val targets: Lens[ProgramSummaries, TargetList]           = Focus[ProgramSummaries](_.targets)
  val observations: Lens[ProgramSummaries, ObservationList] =
    Focus[ProgramSummaries](_.observations)
