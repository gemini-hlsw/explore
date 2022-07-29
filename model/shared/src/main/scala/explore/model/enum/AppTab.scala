// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

/**
 * Describes the application tab buttons in the sidebar
 *
 * @param title
 *   The text for the button
 * @param buttonGroup
 *   Groups the buttons with the same value together
 *
 * Within a button group, order is determined by the AppTab Order instance, which is determined by
 * the order in AppTab.all.
 */
sealed abstract class AppTab(
  val title:       String,
  val buttonGroup: Int
) extends Product
    with Serializable

object AppTab {
  case object Proposal       extends AppTab("Proposal", 1)
  case object Overview       extends AppTab("Overview", 2)
  case object Observations   extends AppTab("Observations", 3)
  case object Targets        extends AppTab("Targets", 3)
  case object Configurations extends AppTab("Configurations", 3)
  case object Constraints    extends AppTab("Constraints", 3)

  val all =
    NonEmptyList.of(Proposal, Overview, Observations, Targets, /*Configurations,*/ Constraints)

  /** @group Typeclass Instances */
  implicit val AppTabEnumerated: Enumerated[AppTab] =
    Enumerated.of(all.head, all.tail: _*)
}
