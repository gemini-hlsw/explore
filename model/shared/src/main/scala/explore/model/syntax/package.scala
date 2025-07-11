// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.*
import explore.model.enums.PosAngleOptions
import lucuma.core.enums.AttachmentPurpose
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode

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

  extension (at: AttachmentType)
    def accept: String = at.fileExtensions.toList.sorted.map("." + _.value).mkString(",")

  val customSedIdOptional = SourceProfile.unnormalizedSED.some
    .andThen(UnnormalizedSED.userDefinedAttachment)
    .andThen(UnnormalizedSED.UserDefinedAttachment.attachmentId)

  extension (eat: Enumerated[AttachmentType])
    def forPurpose(purpose: AttachmentPurpose): List[AttachmentType]       =
      eat.all.filter(_.purpose === purpose)
    def notForPurposes(purposes: AttachmentPurpose*): List[AttachmentType] =
      eat.all.filterNot(at => purposes.contains(at.purpose))

  extension [A](e: Enumerated[A])
    def without(purposes: Set[A]): List[A] =
      e.all.filterNot(purposes.contains)

  extension (self: SourceProfile)
    def customSedId: Option[Attachment.Id] = customSedIdOptional.getOption(self)

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
    def ongoingOf(obsIds: ObsIdSet): Option[ObsIdSet]                                        =
      val ongoing = obsIds.idSet.filter(id => observations.get(id).fold(false)(_.isOngoing))
      ObsIdSet.fromSortedSet(ongoing)
    def completedOf(obsIds: ObsIdSet): Option[ObsIdSet]                                      =
      val completed = obsIds.idSet.filter(id => observations.get(id).fold(false)(_.isCompleted))
      ObsIdSet.fromSortedSet(completed)
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

  extension (self: AttachmentList)
    def listForPurpose(purpose: AttachmentPurpose): List[Attachment]  =
      self.map(_._2).filter(_.isForPurpose(purpose)).toList
    def proposalList: List[Attachment]                                =
      listForPurpose(AttachmentPurpose.Proposal)
    def listForType(attachmentType: AttachmentType): List[Attachment] =
      self.map(_._2).filter(_.attachmentType === attachmentType).toList
    def finderList: List[Attachment]                                  =
      listForType(AttachmentType.Finder)
    def hasForType(attachmentType: AttachmentType): Boolean           =
      self.exists(_._2.attachmentType === attachmentType)

  extension [A](list: Iterable[A])
    def toSortedMap[K: Ordering, V](getKey: A => K, getValue: A => V = identity[A](_)) =
      SortedMap.from(list.map(a => (getKey(a), getValue(a))))

  // TODO Move to core
  extension (site: Site)
    def inPreferredDeclination(d: Declination): Boolean =
      d.toAngle.toSignedDoubleDegrees match
        case d if d < -37.0 => site === Site.GS
        case d if d > 28.0  => site === Site.GN
        case _              => true

  extension (cs: ConstraintSet)
    def summaryString: String =
      s"${cs.imageQuality.toImageQuality.label} ${cs.cloudExtinction.toCloudExtinction.label} ${cs.skyBackground.label} ${cs.waterVapor.label}"

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

  extension (om: ObservingMode)
    def centralWavelength: Option[CentralWavelength] =
      ObservingMode.gmosNorthLongSlit
        .andThen(ObservingMode.GmosNorthLongSlit.centralWavelength)
        .getOption(om)
        .orElse(
          ObservingMode.gmosSouthLongSlit
            .andThen(ObservingMode.GmosSouthLongSlit.centralWavelength)
            .getOption(om)
        )
        .orElse(
          // FIXME: Hardcoded for Flamingos2
          ObservingMode.flamingos2LongSlit
            .getOption(om)
            .flatMap(f => CentralWavelength(f.filter.wavelength).some)
        )

  extension (bc: BasicConfiguration)
    def centralWavelength: Option[CentralWavelength] = bc match
      case g: BasicConfiguration.GmosNorthLongSlit  =>
        g.centralWavelength.some
      case g: BasicConfiguration.GmosSouthLongSlit  =>
        g.centralWavelength.some
      case g: BasicConfiguration.Flamingos2LongSlit =>
        CentralWavelength(g.filter.wavelength).some
      case g: BasicConfiguration.GmosNorthImaging   =>
        CentralWavelength(g.filter.head.wavelength).some
      case g: BasicConfiguration.GmosSouthImaging   =>
        CentralWavelength(g.filter.head.wavelength).some

  extension (bc: ObservingModeType)
    def defaultPosAngleOptions: PosAngleOptions =
      bc.defaultPosAngleConstraint match
        case PosAngleConstraint.Unbounded              => PosAngleOptions.Unconstrained
        case PosAngleConstraint.Fixed(_)               => PosAngleOptions.Fixed
        case PosAngleConstraint.AllowFlip(_)           => PosAngleOptions.AllowFlip
        case PosAngleConstraint.ParallacticOverride(_) => PosAngleOptions.ParallacticOverride
        case PosAngleConstraint.AverageParallactic     => PosAngleOptions.AverageParallactic

  extension (pac: PosAngleConstraint)
    def fallbackPosAngle(averagePA: Option[Angle]): Angle =
      pac match
        case PosAngleConstraint.Fixed(a)               => a
        case PosAngleConstraint.AllowFlip(a)           => a
        case PosAngleConstraint.ParallacticOverride(a) => a
        case PosAngleConstraint.Unbounded              => Angle.Angle0
        case PosAngleConstraint.AverageParallactic     =>
          averagePA.getOrElse(Angle.Angle0)

  extension [A](calc: CalculatedValue[A])
    def isReady: Boolean = calc.state === CalculationState.Ready
    def isStale: Boolean = !isReady

  extension [A](a: A) def asReady: CalculatedValue[A] = CalculatedValue(CalculationState.Ready, a)

  extension (calcDigest: CalculatedValue[Option[ExecutionDigest]])
    def programTimeEstimate: CalculatedValue[Option[TimeSpan]] =
      calcDigest.map(_.map(_.fullTimeEstimate.programTime))
    def fullSetupTime: CalculatedValue[Option[TimeSpan]]       = calcDigest.map(_.map(_.setup.full))
    def remainingObsTime: CalculatedValue[Option[TimeSpan]]    =
      calcDigest.map(_.map(d => d.science.timeEstimate.sum +| d.setup.full))
