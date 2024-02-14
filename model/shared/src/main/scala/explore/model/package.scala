// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order.given
import cats.syntax.all.*
import crystal.Pot
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.KeyedIndexedList
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.core.util.NewType
import lucuma.refined.*

import java.util.UUID
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
type SchedulingGroupList        = SortedMap[ObsIdSet, List[TimingWindow]]
type ObsAttachmentList          = SortedMap[ObsAtt.Id, ObsAttachment]
type ObsAttachmentAssignmentMap = Map[ObsAtt.Id, SortedSet[Observation.Id]]
type ProgramInfoList            = SortedMap[Program.Id, ProgramInfo]

type GroupList = List[GroupElement]

object ObservationExecutionMap extends NewType[Map[Observation.Id, (UUID, Pot[Execution])]]:
  extension (t: Type)
    def getPot(obsId: Observation.Id): Pot[Execution]                                            =
      t.value.get(obsId).map(_._2).getOrElse(Pot.pending)
    def getUUID(obsId: Observation.Id): Option[UUID]                                             =
      t.value.get(obsId).map(_._1)
    def updated(obsId: Observation.Id, uuid: UUID, pot: Pot[Execution]): ObservationExecutionMap =
      ObservationExecutionMap(t.value.updated(obsId, (uuid, pot)))
    def updatePending(obsId: Observation.Id, uuid: UUID): ObservationExecutionMap                =
      updated(obsId, uuid, Pot.pending)

type ObservationExecutionMap = ObservationExecutionMap.Type
