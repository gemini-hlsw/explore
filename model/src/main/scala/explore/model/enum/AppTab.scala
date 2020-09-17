// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

sealed abstract class AppTab(val title: String, val buttonGroup: Int)
    extends Product
    with Serializable

object AppTab {
  case object Overview       extends AppTab("Overview", 1)
  case object Observations   extends AppTab("Observations", 2)
  case object Targets        extends AppTab("Targets", 2)
  case object Configurations extends AppTab("Configurations", 2)
  case object Constraints    extends AppTab("Constraints", 2)

  val all = NonEmptyList.of(Overview, Observations, Targets, Configurations, Constraints)

  /** @group Typeclass Instances */
  implicit val AppTabEnumerated: Enumerated[AppTab] =
    Enumerated.of(all.head, all.tail: _*)
}
