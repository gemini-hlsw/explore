// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.Enumerated

sealed abstract class AppTab(
  val title:       String,
  val buttonGroup: ButtonGroup,
  val groupOrder:  PosInt
) extends Product
    with Serializable

object AppTab {
  import ButtonGroup._

  case object Overview       extends AppTab("Overview", OverviewButton, 1)
  case object Observations   extends AppTab("Observations", ObservationGroup, 1)
  case object Targets        extends AppTab("Targets", ObservationGroup, 2)
  case object Configurations extends AppTab("Configurations", ObservationGroup, 3)
  case object Constraints    extends AppTab("Constraints", ObservationGroup, 4)

  val all = NonEmptyList.of(Overview, Observations, Targets, Configurations, Constraints)

  /** @group Typeclass Instances */
  implicit val AppTabEnumerated: Enumerated[AppTab] =
    Enumerated.of(all.head, all.tail: _*)
}

sealed abstract class ButtonGroup extends Product with Serializable

object ButtonGroup {
  case object OverviewButton   extends ButtonGroup
  case object ObservationGroup extends ButtonGroup

  // This defines the ordering of the groups.
  val all = NonEmptyList.of(OverviewButton, ObservationGroup)

  implicit val ButtonGroupEnumerated: Enumerated[ButtonGroup] =
    Enumerated.of(all.head, all.tail: _*)
}
