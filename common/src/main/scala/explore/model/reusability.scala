// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Reusability
import gem.util.Enumerated
import gem.Observation

/**
  * Reusability instances for model classes
  */
object reusability {
  implicit val obsIdReuse: Reusability[Observation.Id]            = Reusability.by(_.format)
  implicit val siderealTargetReuse: Reusability[SiderealTarget]   = Reusability.byEq
  implicit val expTargetReuse: Reusability[ExploreSiderealTarget] = Reusability.derive
  implicit def enumReuse[A: Enumerated]: Reusability[A]           =
    Reusability.by(implicitly[Enumerated[A]].tag)
  implicit val conditionsReuse: Reusability[Conditions]           = Reusability.derive
}
