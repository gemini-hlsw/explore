package explore.model.enum

import gem.util.Enumerated
import cats.data.NonEmptyList

sealed abstract class AppTab(val title: String) extends Product with Serializable

object AppTab {
  case object Overview       extends AppTab("Overview")
  case object Observations   extends AppTab("Observations")
  case object Target         extends AppTab("Target")
  case object Configurations extends AppTab("Configurations")
  case object Constraints    extends AppTab("Constraints")

  val all = NonEmptyList.of(Overview, Observations, Target, Configurations, Constraints)

  /** @group Typeclass Instances */
  implicit val AppTabEnumerated: Enumerated[AppTab] =
    Enumerated.of(all.head, all.tail: _*)
}
