// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.time.Duration

import clue.StreamingClientStatus
import explore.data.KeyedIndexedList
import gem.Observation
import gem.data.EnumZipper
import gem.util.Enumerated
import gpp.ui.reusability._
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.raw.JsNumber
import react.common.implicits._

/**
  * Reusability instances for model classes
  */
object reusability {
  implicit val statusReuse: Reusability[StreamingClientStatus]             = Reusability.derive
  implicit val durationReuse: Reusability[Duration]                        = Reusability.by(_.getSeconds)
  implicit val siderealTargetReuse: Reusability[SiderealTarget]            = Reusability.byEq
  implicit val targetOptionsReuse: Reusability[TargetVisualOptions]        = Reusability.derive
  implicit val expTargetReuse: Reusability[ExploreSiderealTarget]          = Reusability.derive
  implicit val expObsReuse: Reusability[ExploreObservation]                = Reusability.derive
  implicit def enumReuse[A: Enumerated]: Reusability[A]                    =
    Reusability.by(Enumerated[A].tag)
  implicit def enumZipperReuse[A: Reusability]: Reusability[EnumZipper[A]]                        =
    Reusability.by(z => (z.lefts, z.focus, z.rights))
  implicit val jsNumberReuse: Reusability[JsNumber]                                               = Reusability.byEq
  implicit val focusedReuse: Reusability[Focused]                                                 = Reusability.derive
  implicit val rootModelReuse: Reusability[RootModel]                                             = Reusability.derive
  implicit def idListReuse[Id: Reusability, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)
  implicit def targetSummaryReuse: Reusability[TargetSummary]                                     = Reusability.derive
  implicit def constraintsSummaryReuse: Reusability[ConstraintsSummary]                           = Reusability.derive
  implicit def obsSummaryReuse: Reusability[ObsSummary]                                           = Reusability.derive
}
