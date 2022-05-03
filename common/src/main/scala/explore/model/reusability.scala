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
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.ui.reusability._

import scala.collection.immutable.TreeSeqMap

/**
 * Reusability instances for model classes
 */
object reusability {
  // Model
  implicit val itcTargetProps: Reusability[ITCTarget]                                = Reusability.byEq
  implicit def appContextReuse[F[_]]: Reusability[AppContext[F]]                     = Reusability.always
  implicit val statusReuse: Reusability[PersistentClientStatus]                      = Reusability.byEq
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                  = Reusability.byEq
  implicit val userVaultReuse: Reusability[UserVault]                                = Reusability.byEq
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                  = Reusability.byEq
  implicit val rootModelReuse: Reusability[RootModel]                                = Reusability.byEq
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)

  implicit val scienceTargetsReuse: Reusability[TreeSeqMap[Target.Id, Target]]        =
    Reusability.by((_: TreeSeqMap[Target.Id, Target]).toMap)(Reusability.map)
  implicit val obsIdSetReuse: Reusability[ObsIdSet]                                   = Reusability.byEq
  implicit val targetIdSetReuse: Reusability[TargetIdSet]                             = Reusability.byEq
  implicit val targetWithIdReuse: Reusability[TargetWithId]                           = Reusability.byEq
  implicit val targetWithOptIdReuse: Reusability[TargetWithOptId]                     = Reusability.byEq
  implicit val targetGroupReuse: Reusability[TargetGroup]                             = Reusability.byEq
  implicit val constraintsSummaryReuse: Reusability[ConstraintsSummary]               = Reusability.byEq
  implicit val constraintGroupReuse: Reusability[ConstraintGroup]                     = Reusability.byEq
  implicit val proposalDetailsReuse: Reusability[ProposalDetails]                     = Reusability.byEq
  implicit val partnerSplitReuse: Reusability[PartnerSplit]                           = Reusability.byEq
  implicit val obsSummaryReuse: Reusability[ObsSummary]                               = Reusability.byEq
  implicit val localPreferencesReuse: Reusability[ExploreLocalPreferences]            = Reusability.byEq
  implicit val posAngleReuse: Reusability[PosAngle]                                   = Reusability.byEq
  implicit val obsSummaryWithConstraintsReuse: Reusability[ObsSummaryWithConstraints] =
    Reusability.byEq
  implicit val obsSummaryWithTargetsAndConstraintsReuse
    : Reusability[ObsSummaryWithTitleAndConstraints] = Reusability.byEq
  implicit val obsSummaryWithTargetsConstraintsAndConfReuse
    : Reusability[ObsSummaryWithTitleConstraintsAndConf] = Reusability.byEq

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
  implicit def modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]]           = Reusability.byEq

  implicit val filterReuse: Reusability[AvailableFilter]              = Reusability.byEq
  implicit val optionsReuse: Reusability[ImagingConfigurationOptions] = Reusability.byEq
  implicit val percentageReuse: Reusability[Progress]                 = Reusability.byEq

  implicit val angularSizeReuse: Reusability[AngularSize]                 = Reusability.byEq
  implicit val catalogTargetResultReuse: Reusability[CatalogTargetResult] = Reusability.byEq
  implicit val scienceModenResultReuse: Reusability[ScienceModeBasic]     = Reusability.byEq
  implicit val obsCOnfigurationReuse: Reusability[ObsConfiguration]       = Reusability.byEq

  implicit val existenceReuse: Reusability[Existence] = Reusability.byEq
}
