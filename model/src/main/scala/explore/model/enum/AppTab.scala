// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

/**
 * Describes the application tab buttons in the sidebar
 *
 * @param title The text for the button
 * @param buttonGroup Groups the buttons with the same value together
 *
 * Within a button group, order is determined by the AppTab Order instance,
 * which is determined by the order in AppTab.all.
 */
sealed abstract class AppTab(
  val title:       String,
  val buttonGroup: Int
) extends Product
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
