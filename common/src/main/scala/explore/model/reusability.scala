// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.time.Duration

import clue.StreamingClientStatus
import gem.Observation
import gem.Observation
import gem.util.Enumerated
import gem.util.Enumerated
import gpp.util.EnumZipper
import gpp.util.Zipper
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
  implicit val obsIdReuse: Reusability[Observation.Id]                     = Reusability.by(_.format)
  implicit val targetIdReuse: Reusability[SiderealTarget.Id]               = Reusability.by(_.format)
  implicit val siderealTargetReuse: Reusability[SiderealTarget]            = Reusability.byEq
  implicit val expTargetReuse: Reusability[ExploreSiderealTarget]          = Reusability.derive
  implicit val expObsReuse: Reusability[ExploreObservation]                = Reusability.derive
  implicit def enumReuse[A: Enumerated]: Reusability[A]                    =
    Reusability.by(Enumerated[A].tag)
  implicit val conditionsReuse: Reusability[Conditions]                    = Reusability.derive
  implicit def enumZipperReuse[A: Reusability]: Reusability[EnumZipper[A]] =
    Reusability.by(z => (z.lefts, z.focus, z.rights))
  implicit val jsNumberReuse: Reusability[JsNumber]                        = Reusability.byEq
  implicit def focusedReuse: Reusability[Focused]                          = Reusability.derive
  implicit val reuse: Reusability[RootModel]                               = Reusability.derive
}
