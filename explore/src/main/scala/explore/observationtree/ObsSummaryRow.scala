// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import crystal.Pot
import explore.model.Asterism
import explore.model.Execution
import explore.model.Group
import explore.model.Observation
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithId

import java.time.Instant

// 24 October 2024 - scalafix failing to parse with fewer braces
// Helper ADT for table rows type
enum ObsSummaryRow:
  val obs: Observation

  case ExpandedTargetRow(
    obs:          Observation,
    targetWithId: TargetWithId,
    vizTime:      Option[Instant]
  ) extends ObsSummaryRow

  case ObsRow(
    obs:          Observation,
    targetWithId: Option[TargetWithId],
    asterism:     Option[Asterism],
    group:        Option[Group],
    execution:    Pot[Execution]
  ) extends ObsSummaryRow

  def fold[A](f: ExpandedTargetRow => A, g: ObsRow => A): A =
    this match
      case r: ExpandedTargetRow => f(r)
      case r: ObsRow            => g(r)

  def isLastAsterismTargetOf: Option[Observation.Id] = fold(
    targetRow =>
      Option.when(obs.scienceTargetIds.lastOption.contains_(targetRow.targetWithId.id))(obs.id),
    _ => none
  )

  def coordsAtVizTime: Option[Coordinates] =
    this match
      case r: ExpandedTargetRow => targetCoords(r.targetWithId, r.vizTime)
      case r: ObsRow            =>
        asterismCoords(r.asterism, r.obs.observationTime)
          .orElse(r.targetWithId.flatMap(t => targetCoords(t, r.obs.observationTime)))

  private def targetCoords(twid: TargetWithId, vizTime: Option[Instant]): Option[Coordinates] =
    Target.sidereal
      .getOption(twid.target)
      .flatMap(t => vizTime.fold(t.tracking.baseCoordinates.some)(t.tracking.at))

  private def asterismCoords(
    asterism: Option[Asterism],
    vizTime:  Option[Instant]
  ): Option[Coordinates] =
    asterism
      .map(_.baseTracking)
      .flatMap(bt => vizTime.fold(bt.baseCoordinates.some)(v => bt.at(v).map(_.value)))
