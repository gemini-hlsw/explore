// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order.given
import cats.kernel.Order
import cats.syntax.all.*
import crystal.syntax.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.PerishablePot.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Coordinates
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Group
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.util.NewType
import lucuma.refined.*
import lucuma.schemas.model.ObservingMode
import monocle.Focus
import monocle.Lens

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

object AsterismIds:
  val empty: AsterismIds = SortedSet.empty[Target.Id]

type AsterismGroupList             = SortedMap[ObsIdSet, AsterismIds]
type TargetList                    = SortedMap[Target.Id, Target]
type TargetWithObsList             = SortedMap[Target.Id, TargetWithObs]
type ObservationList               = SortedMap[Observation.Id, Observation]
type GroupList                     = SortedMap[Group.Id, Group]
type ConstraintGroupList           = SortedMap[ObsIdSet, ConstraintSet]
type SchedulingGroupList           = SortedMap[ObsIdSet, List[TimingWindow]]
type ObservingModeGroupList        = SortedMap[ObsIdSet, Option[ObservingMode]]
type AttachmentList                = SortedMap[Attachment.Id, Attachment]
type ObsAttachmentAssignmentMap    = Map[Attachment.Id, SortedSet[Observation.Id]]
type TargetAttachmentAssignmentMap = Map[Attachment.Id, SortedSet[Target.Id]]
type ProgramInfoList               = SortedMap[Program.Id, ProgramInfo]
type ConfigurationRequestList      = SortedMap[ConfigurationRequest.Id, ConfigurationRequestWithObsIds]

type ObservationsAndTargets = (ObservationList, TargetList)

object ObservationsAndTargets:
  val observations: Lens[ObservationsAndTargets, ObservationList] =
    Focus[ObservationsAndTargets](_._1)
  val targets: Lens[ObservationsAndTargets, TargetList]           = Focus[ObservationsAndTargets](_._2)

type PosAngleConstraintAndObsMode = (PosAngleConstraint, Option[ObservingMode])

object PosAngleConstraintAndObsMode:
  val posAngleConstraint: Lens[PosAngleConstraintAndObsMode, PosAngleConstraint] =
    Focus[PosAngleConstraintAndObsMode](_._1)
  val observingMode: Lens[PosAngleConstraintAndObsMode, Option[ObservingMode]]   =
    Focus[PosAngleConstraintAndObsMode](_._2)

object ObservationExecutionMap extends PotMap[Observation.Id, Execution]
type ObservationExecutionMap = ObservationExecutionMap.Type

object GroupTimeRangeMap extends PotMap[Group.Id, Option[ProgramTimeRange]]
type GroupTimeRangeMap = GroupTimeRangeMap.Type

trait PotMap[K, V] extends NewType[Map[K, PerishablePot[V]]]:
  extension (t: Type)
    def getPot(k: K): PerishablePot[V]          =
      t.value.get(k).getOrElse(pending)
    def updated(k: K, v: V): this.Type          =
      apply(t.value.updated(k, PerishablePot(v)))
    def withUpdatePending(k: K): this.Type      =
      apply(t.value.updated(k, pending))
    def markStale(ks: K*): this.Type            =
      apply(ks.foldLeft(t.value)(_.updatedWith(_)(_.map(_.setStale))))
    def removed(k: K): this.Type                =
      apply(t.value.removed(k))
    def setError(k: K, e: Throwable): this.Type =
      apply(t.value.updated(k, PerishablePot.error(e)))

val SupportedInstruments =
  List(Instrument.GmosNorth, Instrument.GmosSouth, Instrument.Flamingos2)

private given Order[ObservingMode.GmosNorthLongSlit] =
  Order.by(x =>
    (x.grating,
     x.filter,
     x.fpu,
     x.centralWavelength,
     x.explicitAmpReadMode.getOrElse(x.defaultAmpReadMode),
     x.explicitRoi.getOrElse(x.defaultRoi)
    )
  )

private given Order[ObservingMode.GmosSouthLongSlit] =
  Order.by(x =>
    (x.grating,
     x.filter,
     x.fpu,
     x.centralWavelength,
     x.explicitAmpReadMode.getOrElse(x.defaultAmpReadMode),
     x.explicitRoi.getOrElse(x.defaultRoi)
    )
  )

given Order[ObservingMode] = Order.from:
  case (a: ObservingMode.GmosNorthLongSlit, b: ObservingMode.GmosNorthLongSlit) =>
    a.compare(b)
  case (a: ObservingMode.GmosSouthLongSlit, b: ObservingMode.GmosSouthLongSlit) =>
    a.compare(b)
  case (_: ObservingMode.GmosNorthLongSlit, _)                                  => -1
  case _                                                                        => 1
