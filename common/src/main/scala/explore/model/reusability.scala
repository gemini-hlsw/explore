// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Reusability

/**
  * Reusability instances for model classes
  */
object reusability {
  implicit val siderealTargetReuse: Reusability[SiderealTarget]   = Reusability.byEq
  implicit val expTargetReuse: Reusability[ExploreSiderealTarget] =
    Reusability.derive[ExploreSiderealTarget]
}
