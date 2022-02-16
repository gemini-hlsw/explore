// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptySet
import clue.PersistentClientStatus
import crystal.react.implicits._
import explore.data.KeyedIndexedList
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.undo.UndoStacks
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Units
import lucuma.core.model.EphemerisKey
import lucuma.core.model.Semester
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.ui.reusability._

import scala.annotation.nowarn
import scala.collection.immutable.TreeSeqMap

/**
 * Reusability instances for model classes
 */
object reusability {
  // Core model - move to lucuma-ui
  implicit val unitsReuse: Reusability[Units] = Reusability.byEq
  @nowarn // Reusability context bound is required but the compiler emits a warning anyway.
  implicit def measureReuse[N: Reusability]: Reusability[Measure[N]]          = Reusability.derive
  implicit def spectralDefinitionReuse[T]: Reusability[SpectralDefinition[T]] = Reusability.derive
  implicit val sourceProfileReuse: Reusability[SourceProfile]                 = Reusability.derive
  implicit val targetReuse: Reusability[Target]                               = Reusability.derive
  implicit val semesterReuse: Reusability[Semester]                           = Reusability.derive
  implicit def nonEmptySetReuse[A]: Reusability[NonEmptySet[A]]               =
    Reusability.by(_.toSortedSet.unsorted)

  // Model
  implicit val itcTargetProps: Reusability[ITCTarget]                                 = Reusability.byEq
  implicit def appContextReuse[F[_]]: Reusability[AppContext[F]]                      = Reusability.always
  implicit val targetSummaryReuse: Reusability[TargetSummary]                         = Reusability.derive
  implicit val statusReuse: Reusability[PersistentClientStatus]                       = Reusability.derive
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                   = Reusability.derive
  implicit val userVaultReuse: Reusability[UserVault]                                 = Reusability.derive
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                   = Reusability.derive
  implicit val rootModelReuse: Reusability[RootModel]                                 = Reusability.derive
  implicit val focusedObsReuse: Reusability[FocusedObs]                               = Reusability.derive
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]]  =
    Reusability.by(_.toList)
  implicit val ephemerisKeyReuse: Reusability[EphemerisKey]                           = Reusability.derive
  implicit val SiderealTargetReuse: Reusability[Target.Sidereal]                      = Reusability.derive
  implicit val NonsiderealTargetReuse: Reusability[Target.Nonsidereal]                = Reusability.derive
  implicit val scienceTargetsReuse: Reusability[TreeSeqMap[Target.Id, Target]]        =
    Reusability.by((_: TreeSeqMap[Target.Id, Target]).toMap)(Reusability.map)
  implicit val obsIdSetReuse: Reusability[ObsIdSet]                                   = Reusability.derive
  implicit val targetIdSetReuse: Reusability[TargetIdSet]                             = Reusability.derive
  implicit val targetWithIdReuse: Reusability[TargetWithId]                           = Reusability.derive
  implicit val targetWithOptIdReuse: Reusability[TargetWithOptId]                     = Reusability.derive
  implicit val targetGroupReuse: Reusability[TargetGroup]                             = Reusability.derive
  implicit val airMassRangeReuse: Reusability[AirMassRange]                           = Reusability.derive
  implicit val hourAngleRangeReuse: Reusability[HourAngleRange]                       = Reusability.derive
  implicit val elevationRangeReuse: Reusability[ElevationRange]                       = Reusability.derive
  implicit val constraintsSummaryReuse: Reusability[ConstraintsSummary]               = Reusability.byEq
  implicit val constraintsSetReuse: Reusability[ConstraintSet]                        = Reusability.derive
  implicit val constraintGroupReuse: Reusability[ConstraintGroup]                     = Reusability.derive
  implicit val proposalDetailsReuse: Reusability[ProposalDetails]                     = Reusability.byEq
  implicit val partnerSplitReuse: Reusability[PartnerSplit]                           = Reusability.derive
  implicit val obsSummaryReuse: Reusability[ObsSummary]                               = Reusability.byEq
  implicit val localPreferencesReuse: Reusability[ExploreLocalPreferences]            = Reusability.byEq
  implicit val obsSummaryWithConstraintsReuse: Reusability[ObsSummaryWithConstraints] =
    Reusability.derive
  implicit val obsSummaryWithTargetsAndConstraintsReuse
    : Reusability[ObsSummaryWithTargetsAndConstraints] = Reusability.derive
  implicit def undoStacksReuse[F[_], M]: Reusability[UndoStacks[F, M]]                =
    Reusability.by(s => (s.undo.length, s.redo.length, s.working))
  implicit def undoContextReuse[M: Reusability]: Reusability[UndoContext[M]]          =
    Reusability.by(x => (x.model, x.stacks))

  implicit def undoSetterReuse[M: Reusability]: Reusability[UndoSetter[M]] =
    Reusability.by(_.model)

  implicit def undoStacksMapReuse[F[_], K, M]: Reusability[Map[K, UndoStacks[F, M]]] =
    Reusability.by[Map[K, UndoStacks[F, M]], Int](_.size) && Reusability[Map[K, UndoStacks[F, M]]](
      (a, b) =>
        a.forall { case (k, stacksA) =>
          b.get(k).exists(stacksB => undoStacksReuse.test(stacksA, stacksB))
        }
    )
  implicit def modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]]           = Reusability.derive
  implicit val filterReuse: Reusability[AvailableFilter]                             = Reusability.byEq
  implicit val optionsReuse: Reusability[ImagingConfigurationOptions]                = Reusability.derive
  implicit val percentageReuse: Reusability[Progress]                                = Reusability.derive

  implicit val angularSizeReuse: Reusability[AngularSize]                          = Reusability.derive
  implicit val catalogTargetResultReuse: Reusability[CatalogTargetResult]          = Reusability.derive
  implicit val scienceConfigurationnResultReuse: Reusability[ScienceConfiguration] =
    Reusability.byEq
}
