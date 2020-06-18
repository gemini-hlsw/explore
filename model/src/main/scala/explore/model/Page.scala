// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.implicits._
import gem.Observation

sealed trait Page extends Product with Serializable

object Page {
  case object HomePage           extends Page
  final case class ObsPage(obsId: ExploreObservation.Id) extends Page
  final case class TargetPage(targetId: SiderealTarget.Id) extends Page
  final case class TargetsObsPage(obsId: ExploreObservation.Id) extends Page
  case object ConfigurationsPage extends Page
  case object ConstraintsPage    extends Page

  implicit val eqPage: Eq[Page] = Eq.instance {
    case (HomePage, HomePage)                     => true
    case (ObsPage(a), ObsPage(b))                 => a === b
    case (TargetPage(a), TargetPage(b))           => a === b
    case (TargetsObsPage(a), TargetsObsPage(b))   => a === b
    case (ConfigurationsPage, ConfigurationsPage) => true
    case (ConstraintsPage, ConstraintsPage)       => true
    case _                                        => false
  }
}
