// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import explore.undo.UndoStacks
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.ui.reusability._

/**
 * Reusability instances for model classes
 */
object reusability {
  // Model
  implicit val pointingReuse: Reusability[Pointing]                                   = Reusability.byEq
  implicit val statusReuse: Reusability[PersistentClientStatus]                       = Reusability.derive
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                   = Reusability.derive
  implicit val userVaultReuse: Reusability[UserVault]                                 = Reusability.byEq
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                   = Reusability.byEq
  implicit val rootModelReuse: Reusability[RootModel]                                 = Reusability.byEq
  implicit val focusedReuse: Reusability[Focused]                                     = Reusability.derive
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]]  =
    Reusability.by(_.toList)
  implicit val airMassRangeReuse: Reusability[AirMassRange]                           = Reusability.byEq
  implicit val hourAngleRangeReuse: Reusability[HourAngleRange]                       = Reusability.byEq
  implicit val elevationRangeReuse: Reusability[ElevationRange]                       = Reusability.byEq
  implicit val constraintsSummaryReuse: Reusability[ConstraintsSummary]               = Reusability.byEq
  implicit val proposalDetailsReuse: Reusability[ProposalDetails]                     = Reusability.byEq
  implicit val partnerSplitReuse: Reusability[PartnerSplit]                           = Reusability.derive
  implicit val obsSummaryReuse: Reusability[ObsSummary]                               = Reusability.byEq
  implicit val obsSummaryWithConstraintsReuse: Reusability[ObsSummaryWithConstraints] =
    Reusability.byEq
  implicit val obsSummaryWithPointingAndConstraintsReuse
    : Reusability[ObsSummaryWithPointingAndConstraints]                               = Reusability.byEq
  implicit def undoStacksReuse[F[_], M]: Reusability[UndoStacks[F, M]]                =
    Reusability.by(s => (s.undo.length, s.redo.length, s.working))
  implicit def undoStacksMapReuse[F[_], K, M]: Reusability[Map[K, UndoStacks[F, M]]]  =
    Reusability.never
  implicit def modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]]            = Reusability.derive
}
