// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order.given
import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.KeyedIndexedList
import explore.model.syntax.all.*
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.core.optics.SplitEpi
import lucuma.core.util.NewType
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import monocle.Iso

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

val MaxHourValue = BigDecimal(1000)
type HourRange = Interval.Closed[0, 1000]
type Hours     = BigDecimal Refined HourRange
object Hours extends RefinedTypeOps[Hours, BigDecimal] {
  val Max: Hours = Hours.unsafeFrom(MaxHourValue)
}

val NewTargetName: NonEmptyString = "<New Target>".refined

val EmptySiderealTarget =
  Target.Sidereal(
    NewTargetName,
    SiderealTracking.const(Coordinates.Zero),
    SourceProfile.Point(SpectralDefinition.BandNormalized(none, SortedMap.empty)),
    none
  )

type AsterismIds = SortedSet[Target.Id]

type AsterismGroupList          = SortedMap[ObsIdSet, AsterismIds]
type TargetList                 = SortedMap[Target.Id, Target]
type TargetWithObsList          = SortedMap[Target.Id, TargetWithObs]
// KeyedIndexedList is only useful is manual order is going to matter.
// For the moment I'm keeping it because it seems it will matter at some point.
// Otherwise, we should change to a SortedMap.
type ObservationList            = KeyedIndexedList[Observation.Id, ObsSummary]
type ConstraintGroupList        = SortedMap[ObsIdSet, ConstraintSet]
type ObsAttachmentList          = SortedMap[ObsAtt.Id, ObsAttachment]
type ObsAttachmentAssignmentMap = Map[ObsAtt.Id, SortedSet[Observation.Id]]
type ProgramInfoList            = SortedMap[Program.Id, ProgramInfo]

type GroupList = List[GroupElement]
