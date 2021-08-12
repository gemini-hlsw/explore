// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.PersistentClientStatus
import crystal.react.implicits._
import explore.data.KeyedIndexedList
import explore.undo.UndoContext
import explore.undo.UndoSetter
import explore.undo.UndoStacks
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Semester
import lucuma.ui.reusability._

/**
 * Reusability instances for model classes
 */
object reusability {
  // Model
  implicit val pointingReuse: Reusability[Pointing]                                            = Reusability.derive
  implicit val statusReuse: Reusability[PersistentClientStatus]                                = Reusability.derive
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                            = Reusability.derive
  implicit val userVaultReuse: Reusability[UserVault]                                          = Reusability.derive
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                            = Reusability.derive
  implicit val rootModelReuse: Reusability[RootModel]                                          = Reusability.derive
  implicit val focusedReuse: Reusability[Focused]                                              = Reusability.derive
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]]           =
    Reusability.by(_.toList)
  implicit val airMassRangeReuse: Reusability[AirMassRange]                                    = Reusability.derive
  implicit val hourAngleRangeReuse: Reusability[HourAngleRange]                                = Reusability.derive
  implicit val elevationRangeReuse: Reusability[ElevationRange]                                = Reusability.derive
  implicit val constraintsSummaryReuse: Reusability[ConstraintsSummary]                        = Reusability.byEq
  implicit val constraintsSetReuse: Reusability[ConstraintSet]                                 = Reusability.derive
  implicit val proposalDetailsReuse: Reusability[ProposalDetails]                              = Reusability.byEq
  implicit val partnerSplitReuse: Reusability[PartnerSplit]                                    = Reusability.derive
  implicit val obsSummaryReuse: Reusability[ObsSummary]                                        = Reusability.byEq
  implicit val obsSummaryWithConstraintsReuse: Reusability[ObsSummaryWithConstraints]          =
    Reusability.derive
  implicit val obsSummaryWithPointingAndConstraintsReuse
    : Reusability[ObsSummaryWithPointingAndConstraints]                                        = Reusability.derive
  implicit def undoStacksReuse[F[_], M]: Reusability[UndoStacks[F, M]]                         =
    Reusability.by(s => (s.undo.length, s.redo.length, s.working))
  implicit def undoContextReuse[F[_], G[_], M: Reusability]: Reusability[UndoContext[F, G, M]] =
    Reusability.by(x => (x.model, x.stacks))

  implicit def undoSetterReuse[F[_], G[_], M: Reusability]: Reusability[UndoSetter[F, G, M]] =
    Reusability.by(_.model)

  implicit def undoStacksMapReuse[F[_], K, M]: Reusability[Map[K, UndoStacks[F, M]]] =
    Reusability.never
  implicit def modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]]           = Reusability.derive
  // Move to lucuma-ui
  implicit val semesterReuse: Reusability[Semester]                                  = Reusability.derive
}
