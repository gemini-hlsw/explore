// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import explore.undo.UndoStacks
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.model.Target
import lucuma.ui.reusability._

import scala.collection.immutable.TreeSeqMap

/**
 * Reusability instances for model classes
 */
object reusability {
  // Model
  implicit val itcTargetProps: Reusability[ITCTarget]                                = Reusability.byEq
  implicit def appContextReuse[F[_]]: Reusability[AppContext[F]]                     = Reusability.always
  implicit val targetSummaryReuse: Reusability[TargetSummary]                        = Reusability.derive
  implicit val statusReuse: Reusability[PersistentClientStatus]                      = Reusability.derive
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                  = Reusability.derive
  implicit val userVaultReuse: Reusability[UserVault]                                = Reusability.derive
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                  = Reusability.derive
  implicit val rootModelReuse: Reusability[RootModel]                                = Reusability.derive
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)

  implicit val scienceTargetsReuse: Reusability[TreeSeqMap[Target.Id, Target]]        =
    Reusability.by((_: TreeSeqMap[Target.Id, Target]).toMap)(Reusability.map)
  implicit val obsIdSetReuse: Reusability[ObsIdSet]                                   = Reusability.derive
  implicit val targetIdSetReuse: Reusability[TargetIdSet]                             = Reusability.derive
  implicit val targetWithIdReuse: Reusability[TargetWithId]                           = Reusability.derive
  implicit val targetWithOptIdReuse: Reusability[TargetWithOptId]                     = Reusability.derive
  implicit val targetGroupReuse: Reusability[TargetGroup]                             = Reusability.derive
  implicit val constraintsSummaryReuse: Reusability[ConstraintsSummary]               = Reusability.byEq
  implicit val constraintGroupReuse: Reusability[ConstraintGroup]                     = Reusability.derive
  implicit val proposalDetailsReuse: Reusability[ProposalDetails]                     = Reusability.byEq
  implicit val partnerSplitReuse: Reusability[PartnerSplit]                           = Reusability.derive
  implicit val obsSummaryReuse: Reusability[ObsSummary]                               = Reusability.byEq
  implicit val localPreferencesReuse: Reusability[ExploreLocalPreferences]            = Reusability.byEq
  implicit val obsSummaryWithConstraintsReuse: Reusability[ObsSummaryWithConstraints] =
    Reusability.derive
  implicit val obsSummaryWithTargetsAndConstraintsReuse
    : Reusability[ObsSummaryWithTargetsAndConstraints] = Reusability.derive

  // Undo
  implicit def undoStacksReuse[F[_], M]: Reusability[UndoStacks[F, M]]               =
    Reusability.by(s => (s.undo.length, s.redo.length, s.working))
  implicit def undoStacksMapReuse[F[_], K, M]: Reusability[Map[K, UndoStacks[F, M]]] =
    Reusability.by[Map[K, UndoStacks[F, M]], Int](_.size) && Reusability[Map[K, UndoStacks[F, M]]](
      (a, b) =>
        a.forall { case (k, stacksA) =>
          b.get(k).exists(stacksB => undoStacksReuse.test(stacksA, stacksB))
        }
    )
  implicit def modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]]           = Reusability.derive

  implicit val filterReuse: Reusability[AvailableFilter]              = Reusability.byEq
  implicit val optionsReuse: Reusability[ImagingConfigurationOptions] = Reusability.derive
  implicit val percentageReuse: Reusability[Progress]                 = Reusability.derive

  implicit val angularSizeReuse: Reusability[AngularSize]                          = Reusability.derive
  implicit val catalogTargetResultReuse: Reusability[CatalogTargetResult]          = Reusability.derive
  implicit val scienceConfigurationnResultReuse: Reusability[ScienceConfiguration] =
    Reusability.byEq
}
