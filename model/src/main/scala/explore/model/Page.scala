// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import gem.Observation
import cats.Eq
import cats.implicits._

sealed trait Page extends Product with Serializable

object Page {
  case object HomePage        extends Page
  case object ConstraintsPage extends Page
  final case class ObsPage(obsId: Observation.Id) extends Page

  implicit val eqPage: Eq[Page] = Eq.instance {
    case (HomePage, HomePage)               => true
    case (ConstraintsPage, ConstraintsPage) => true
    case (ObsPage(a), ObsPage(b))           => a === b
    case _                                  => false
  }
}
