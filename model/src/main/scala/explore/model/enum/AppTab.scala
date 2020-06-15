// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import gem.util.Enumerated
import cats.data.NonEmptyList

sealed abstract class AppTab(val title: String) extends Product with Serializable

object AppTab {
  case object Overview       extends AppTab("Overview")
  case object Observations   extends AppTab("Observations")
  case object Targets        extends AppTab("Targets")
  case object Configurations extends AppTab("Configurations")
  case object Constraints    extends AppTab("Constraints")

  val all = NonEmptyList.of(Overview, Observations, Targets, Configurations, Constraints)

  /** @group Typeclass Instances */
  implicit val AppTabEnumerated: Enumerated[AppTab] =
    Enumerated.of(all.head, all.tail: _*)
}
