// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Key
import japgolly.scalajs.react.Reusability
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.ui.reusability._
import react.common.Size

/**
 * Reusability instances for model classes
 */
object reusability {
  implicit val statusReuse: Reusability[PersistentClientStatus]                       = Reusability.derive
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                   = Reusability.derive
  implicit val userVaultReuse: Reusability[UserVault]                                 = Reusability.byEq
  implicit val targetViewExpandedIdsReuse: Reusability[ExpandedIds]                   = Reusability.byEq
  implicit val rootModelReuse: Reusability[RootModel]                                 = Reusability.byEq
  implicit val sizeReuse: Reusability[Size]                                           = Reusability.by(x => (x.height, x.width))
  implicit val focusedReuse: Reusability[Focused]                                     = Reusability.derive
  implicit def idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]]  =
    Reusability.by(_.toList)
  implicit val airMassRangeReuse: Reusability[AirMassRange]                           = Reusability.byEq
  implicit val hourAngleRangeReuse: Reusability[HourAngleRange]                       = Reusability.byEq
  implicit val elevationRangeReuse: Reusability[ElevationRange]                       = Reusability.byEq
  implicit val constraintsSummaryReuse: Reusability[ConstraintsSummary]               = Reusability.byEq
  implicit val proposalDetailsReuse: Reusability[ProposalDetails]                     = Reusability.byEq
  implicit val partnerSplitReuse: Reusability[PartnerSplit]                           = Reusability.derive
  implicit val pointingReuse: Reusability[Pointing]                                   = Reusability.byEq
  implicit val obsSummaryReuse: Reusability[ObsSummary]                               = Reusability.byEq
  implicit val obsSummaryWithConstraintsReuse: Reusability[ObsSummaryWithConstraints] =
    Reusability.byEq
  implicit val obsSummaryWithPointingAndConstraintsReuse
    : Reusability[ObsSummaryWithPointingAndConstraints]                               = Reusability.byEq
  // Move to lucuma-ui
  implicit val bigDecimalReuse: Reusability[BigDecimal]                               = Reusability.byEq
  implicit val offsetReuse: Reusability[Offset]                                       = Reusability.byEq
  implicit val wavelengthReuse: Reusability[Wavelength]                               = Reusability.byEq
  implicit val keyReuse: Reusability[Key]                                             = Reusability.by_==
}
