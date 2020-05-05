package explore.model

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.CatsReact._
import gem.Target

/**
  * Reusability instances for model classes
  */
object reusability {
  implicit val targetReuse: Reusability[Target]           = Reusability.byEq
  implicit val expTargetReuse: Reusability[ExploreTarget] = Reusability.derive[ExploreTarget]
}
