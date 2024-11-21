// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.*
import explore.model.enums.PosAngleOptions
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.util.Timestamp
import lucuma.schemas.model.BasicConfiguration

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
    // find the first group which contains the entirety of obsIds
    def findContainingObsIds(obsIds: ObsIdSet): Option[AsterismGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(AsterismGroup.fromTuple)

    // find all the groups that contain any of the obsIds
    def filterForObsInSet(obsIds: ObsIdSet): AsterismGroupList =
      self.filterNot(_._1.idSet.intersect(obsIds.idSet).isEmpty)

    def findWithTargetIds(targetIds: SortedSet[Target.Id]): Option[AsterismGroup] =
      self.find { case (_, grpIds) => grpIds === targetIds }.map(AsterismGroup.fromTuple)

  extension (observations: ObservationList)
    def executedOf(obsIds: ObsIdSet): Option[ObsIdSet]                                       =
      val executed = obsIds.idSet.filter(id => observations.get(id).fold(false)(_.isExecuted))
      ObsIdSet.fromSortedSet(executed)
    def addTargetToObservations(targetId: Target.Id, obsIds: ObsIdSet): ObservationList      =
      obsIds.idSet.foldLeft(observations): (map, obsId) =>
        map.updatedWith(obsId)(_.map(Observation.scienceTargetIds.modify(_ + targetId)))
    def removeTargetFromObservations(targetId: Target.Id, obsIds: ObsIdSet): ObservationList =
      obsIds.idSet.foldLeft(observations): (map, obsId) =>
        map.updatedWith(obsId)(_.map(Observation.scienceTargetIds.modify(_ - targetId)))
    def allWithTarget(targetId: Target.Id): Set[Observation.Id]                              =
      observations.values.toList
        .filter(_.scienceTargetIds.contains(targetId))
        .map(_.id)
        .toSet
    // determine if the target is in any other observations other than the ones in obsIds
    def isTargetInOtherObs(targetId: Target.Id, obsIds: ObsIdSet): Boolean                   =
      (allWithTarget(targetId) -- obsIds.idSet.toSortedSet).nonEmpty

  extension (self: ConstraintGroupList)
    @targetName("findContainingObsIdsCS")
    def findContainingObsIds(obsIds: ObsIdSet): Option[ConstraintGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(ConstraintGroup.fromTuple)

    def findWithConstraintSet(constraintSet: ConstraintSet): Option[ConstraintGroup] =
      self.find { case (_, cs) => cs === constraintSet }.map(ConstraintGroup.fromTuple)

  extension (self: SchedulingGroupList)
    @targetName("findContainingObsIdsScheduling")
    def findContainingObsIds(obsIds: ObsIdSet): Option[SchedulingGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(SchedulingGroup.fromTuple)

    def findWithSchedulingGroup(schedulingGroup: List[TimingWindow]): Option[SchedulingGroup] =
      self.find { case (_, sg) => sg === schedulingGroup }.map(SchedulingGroup.fromTuple)

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

  extension (cs: ConstraintSet)
    def summaryString: String =
      s"${cs.imageQuality.label} ${cs.cloudExtinction.label} ${cs.skyBackground.label} ${cs.waterVapor.label}"

  extension (ts: Timestamp)
    def formatUtc: String =
      val i = ts.toInstant.atOffset(ZoneOffset.UTC)
      s"${Constants.GppDateFormatter.format(i)} @ ${Constants.GppTimeTZFormatter.format(i)}"

    def formatUtcWithZone: String =
      val i = ts.toInstant.atOffset(ZoneOffset.UTC)
      s"${Constants.GppDateFormatter.format(i)} @ ${Constants.GppTimeTZFormatterWithZone.format(i)}"

  extension (t: IO.type) def now(): IO[Instant] = IO(Instant.now)

  extension (e: NonNegShort) def toNonNegInt: NonNegInt = NonNegInt.unsafeFrom(e.value)

  extension (cr: CalibrationRole)
    def needsAGS: Boolean = cr match
      case CalibrationRole.SpectroPhotometric => true
      case CalibrationRole.Twilight           => false
      case _                                  => true

    def needsITC: Boolean = cr match
      case CalibrationRole.Twilight => false
      case _                        => true

  extension (cr: Option[CalibrationRole]) def needsITC: Boolean = cr.fold(true)(_.needsITC)

  extension (bc: BasicConfiguration)
    // Currently, everything is long slit and defaults to Average Parallactic.
    // But as we get new modes, Shortcut 3360 states:
    // Slit spectroscopy ->  Average Parallactic
    // MOS -> Fixed
    // Imaging -> Unconstrained
    // IFU -> 180 Flip
    def defaultPosAngleConstrait: PosAngleOptions = bc match
      case BasicConfiguration.GmosNorthLongSlit(_, _, _, _) => PosAngleOptions.AverageParallactic
      case BasicConfiguration.GmosSouthLongSlit(_, _, _, _) => PosAngleOptions.AverageParallactic
