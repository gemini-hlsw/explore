// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import explore.Icons
import explore.model.Asterism
import explore.model.Group
import explore.model.Observation
import explore.model.extensions.*
import explore.syntax.ui.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.model.Target
import lucuma.react.fa.FontAwesomeIcon
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
    group:        Option[Group]
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

  // TODO: Update to handle ToOs/Regions
  def coordsOrRegion: Option[Either[Coordinates, Region]] =
    this match
      case r: ExpandedTargetRow => targetCoordsOrRegion(r.targetWithId, r.vizTime)
      case r: ObsRow            =>
        asterismCoordsOrRegion(r.asterism, r.obs.observationTime)

  def icon: Option[FontAwesomeIcon] = fold(
    e => e.targetWithId.target.icon.some,
    o =>
      o.asterism.map(a =>
        if (a.asNel.isMixed) Icons.Stars
        else a.focus.target.icon
      )
  )

  private def targetCoordsOrRegion(
    twid:    TargetWithId,
    vizTime: Option[Instant]
  ): Option[Either[Coordinates, Region]] =
    vizTime.fold(twid.target)(twid.target.at).coordsOrRegion

  private def asterismCoordsOrRegion(
    asterism: Option[Asterism],
    vizTime:  Option[Instant]
  ): Option[Either[Coordinates, Region]] =
    asterism.flatMap(_.asNel.coordsOrRegionAt(vizTime))
