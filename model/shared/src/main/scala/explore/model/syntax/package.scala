// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.*
import explore.model.enums.PosAngleOptions
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

import java.time.Instant
import java.time.ZoneOffset
import scala.annotation.targetName
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object all:

  extension (pa: PosAngleOptions)
    def toPosAngle(a: Angle): PosAngleConstraint = pa match
      case PosAngleOptions.Fixed               => PosAngleConstraint.Fixed(a)
      case PosAngleOptions.AllowFlip           => PosAngleConstraint.AllowFlip(a)
      case PosAngleOptions.AverageParallactic  => PosAngleConstraint.AverageParallactic
      case PosAngleOptions.ParallacticOverride => PosAngleConstraint.ParallacticOverride(a)
      case PosAngleOptions.Unconstrained       => PosAngleConstraint.Unbounded

  extension (pa: PosAngleConstraint)
    def toPosAngleOptions: PosAngleOptions = pa match
      case PosAngleConstraint.Fixed(_)               => PosAngleOptions.Fixed
      case PosAngleConstraint.AllowFlip(_)           => PosAngleOptions.AllowFlip
      case PosAngleConstraint.AverageParallactic     => PosAngleOptions.AverageParallactic
      case PosAngleConstraint.ParallacticOverride(_) => PosAngleOptions.ParallacticOverride
      case PosAngleConstraint.Unbounded              => PosAngleOptions.Unconstrained

  extension (self: AsterismGroupList)
    def findContainingObsIds(obsIds: ObsIdSet): Option[AsterismGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(AsterismGroup.fromTuple)

    def findWithTargetIds(targetIds: SortedSet[Target.Id]): Option[AsterismGroup] =
      self.find { case (_, grpIds) => grpIds === targetIds }.map(AsterismGroup.fromTuple)

  extension (self: ConstraintGroupList)
    @targetName("findContainingObsIdsCS")
    def findContainingObsIds(obsIds: ObsIdSet): Option[ConstraintGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(ConstraintGroup.fromTuple)

  extension (self: SchedulingGroupList)
    @targetName("findContainingObsIdsScheduling")
    def findContainingObsIds(obsIds: ObsIdSet): Option[SchedulingGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(SchedulingGroup.fromTuple)

  extension [A](list: Iterable[A])
    def toSortedMap[K: Ordering, V](getKey: A => K, getValue: A => V = identity[A](_)) =
      SortedMap.from(list.map(a => (getKey(a), getValue(a))))

  // TODO Move to core
  extension (site: Site)
    def inPreferredDeclination(d: Declination): Boolean =
      d.toAngle.toSignedDoubleDegrees match
        case d if d < -40.0 => site === Site.GS
        case d if d > 30.0  => site === Site.GN
        case _              => true

  extension (timespan: TimeSpan)
    /**
     * Format a timespan in the format `${hh}hrs ${mm}mins`
     */
    def toHoursMinutes: String =
      val hours   = timespan.toHours.toLong
      val minutes = (timespan.toMinutes.toLong) % 60

      if hours === 0 then s"${minutes}mins"
      else if minutes === 0 then s"${hours}hrs"
      else s"${hours}hrs ${minutes}mins"

  extension (cs: ConstraintSet)
    def summaryString: String =
      s"${cs.imageQuality.label} ${cs.cloudExtinction.label} ${cs.skyBackground.label} ${cs.waterVapor.label}"

  extension (ts: Timestamp)
    def formatUtc: String =
      s"${Constants.GppDateFormatter.format(ts.toInstant.atOffset(ZoneOffset.UTC))} @ " +
        s"${Constants.GppTimeTZFormatter.format(ts.toInstant.atOffset(ZoneOffset.UTC))}"

    def formatUtcWithZone: String =
      s"${Constants.GppDateFormatter.format(ts.toInstant.atOffset(ZoneOffset.UTC))} @ " +
        s"${Constants.GppTimeTZFormatterWithZone.format(ts.toInstant.atOffset(ZoneOffset.UTC))}"

  extension (t: IO.type) def now(): IO[Instant] = IO(Instant.now)

  extension (e: Either[GroupObs, GroupingElement])
    def groupIndex: NonNegShort = e.fold(_.groupIndex, _.parentIndex)

  extension (e: Either[GroupObs, Grouping])
    @targetName("groupIndexGrouping")
    def groupIndex: NonNegShort = e.fold(_.groupIndex, _.parentIndex)

  extension (e: GroupTree.Value) def id: GroupTree.Key = e.bimap(_.id, _.id)
