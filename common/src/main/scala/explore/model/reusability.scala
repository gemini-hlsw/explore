package explore.model

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.CatsReact._

/**
  * Reusability instances for model classes
  */
object reusability {
  implicit val siderealTargetReuse: Reusability[SiderealTarget]   = Reusability.byEq
  implicit val expTargetReuse: Reusability[ExploreSiderealTarget] =
    Reusability.derive[ExploreSiderealTarget]
}
