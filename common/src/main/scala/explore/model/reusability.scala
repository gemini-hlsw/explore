// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Reusability
import lucuma.ui.reusability._
import react.common.Size

/**
 * Reusability instances for model classes
 */
object reusability {
  implicit val statusReuse: Reusability[PersistentClientStatus]                      = Reusability.derive
  implicit val siderealTargetReuse: Reusability[SiderealTarget]                      = Reusability.byEq
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                  = Reusability.derive
  implicit val expTargetReuse: Reusability[ExploreSiderealTarget]                    = Reusability.derive
  implicit val constraintsReuse: Reusability[Constraints]                            = Reusability.derive
  implicit val expObsReuse: Reusability[ExploreObservation]                          = Reusability.derive
  implicit val userVaultReuse: Reusability[UserVault]                                = Reusability.byEq
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                  = Reusability.byEq
  implicit val rootModelReuse: Reusability[RootModel]                                = Reusability.byEq
  implicit def sizeReuse: Reusability[Size]                                          = Reusability.by(x => (x.height, x.width))
  implicit def focusedReuse: Reusability[Focused]                                    = Reusability.derive
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)
  implicit def AirMassRangeReuse: Reusability[AirMassRange]                          = Reusability.byEq
  implicit def HourAngleRangeReuse: Reusability[HourAngleRange]                      = Reusability.byEq
  implicit def constraintSetModelReuse: Reusability[ConstraintSetModel]              = Reusability.byEq
  implicit def constraintsSummaryReuse: Reusability[ConstraintsSummary]              = Reusability.byEq
  implicit def obsSummaryReuse: Reusability[ObsSummary]                              = Reusability.byEq
  implicit def proposalDetailsReuse: Reusability[ProposalDetails]                    = Reusability.byEq
  implicit def partnerSplitReuse: Reusability[PartnerSplit]                          = Reusability.derive

}
