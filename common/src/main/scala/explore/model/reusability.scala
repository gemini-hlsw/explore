// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.time.Duration

import clue.StreamingClientStatus
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.KeyedIndexedList
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.raw.JsNumber
import lucuma.core.math.Coordinates
import lucuma.ui.reusability._
import react.common.Size
import react.common.implicits._
import io.circe.Json

/**
 * Reusability instances for model classes
 */
object reusability {
  implicit val nonEmptyStringReuse: Reusability[NonEmptyString]                                   = Reusability.by(_.value)
  implicit val statusReuse: Reusability[StreamingClientStatus]                                    = Reusability.derive
  implicit val durationReuse: Reusability[Duration]                                               = Reusability.by(_.getSeconds)
  implicit val siderealTargetReuse: Reusability[SiderealTarget]                                   = Reusability.byEq
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]                               = Reusability.derive
  implicit val expTargetReuse: Reusability[ExploreSiderealTarget]                                 = Reusability.derive
  implicit val constraintsReuse: Reusability[Constraints]                                         = Reusability.derive
  implicit val expObsReuse: Reusability[ExploreObservation]                                       = Reusability.derive
  implicit val jsNumberReuse: Reusability[JsNumber]                                               = Reusability.byEq
  implicit val rootModelReuse: Reusability[RootModel]                                             = Reusability.derive
  implicit def coordinatesReuse: Reusability[Coordinates]                                         = Reusability.byEq
  implicit def sizeReuse: Reusability[Size]                                                       = Reusability.by(x => (x.height, x.width))
  implicit def focusedReuse: Reusability[Focused]                                                 = Reusability.derive
  implicit def idListReuse[Id: Reusability, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)
  implicit def targetSummaryReuse: Reusability[TargetSummary]                                     = Reusability.derive
  implicit def constraintsSummaryReuse: Reusability[ConstraintsSummary]                           = Reusability.derive
  implicit def obsSummaryReuse: Reusability[ObsSummary]                                           = Reusability.derive
  implicit def proposalDetailsReuse: Reusability[ProposalDetails]                                 = Reusability.derive
  implicit val jsonReuse: Reusability[Json]                                                       = Reusability.by_==
}
