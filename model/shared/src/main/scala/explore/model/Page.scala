// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.model.Observation
import monocle.Iso

sealed trait Page extends Product with Serializable

object Page {
  case object HomePage                                        extends Page
  final case object ProposalPage                              extends Page
  final case object ObservationsBasePage                      extends Page
  final case class ObsPage(obsId: Observation.Id)             extends Page
  final case class ObsAdvancedConfPage(obsId: Observation.Id) extends Page
  final case object TargetsBasePage                           extends Page
  final case class TargetsObsPage(obsId: Observation.Id)      extends Page
  case object ConfigurationsPage                              extends Page
  final case object ConstraintsBasePage                       extends Page
  final case class ConstraintsObsPage(obsId: Observation.Id)  extends Page

  implicit val eqPage: Eq[Page] = Eq.instance {
    case (HomePage, HomePage)                             => true
    case (ProposalPage, ProposalPage)                     => true
    case (ObservationsBasePage, ObservationsBasePage)     => true
    case (ObsPage(a), ObsPage(b))                         => a === b
    case (ObsAdvancedConfPage(a), ObsAdvancedConfPage(b)) => a === b
    case (TargetsBasePage, TargetsBasePage)               => true
    case (TargetsObsPage(a), TargetsObsPage(b))           => a === b
    case (ConfigurationsPage, ConfigurationsPage)         => true
    case (ConstraintsBasePage, ConstraintsBasePage)       => true
    case (ConstraintsObsPage(a), ConstraintsObsPage(b))   => a === b
    case _                                                => false
  }

  object ObsPage {
    final val obsId: Iso[Observation.Id, ObsPage] =
      Iso[Observation.Id, ObsPage](ObsPage.apply)(_.obsId)
  }

  object ObsAdvancedConfPage {
    final val obsId: Iso[Observation.Id, ObsAdvancedConfPage] =
      Iso[Observation.Id, ObsAdvancedConfPage](ObsAdvancedConfPage.apply)(_.obsId)
  }

  object TargetsObsPage {
    final val obsId: Iso[Observation.Id, TargetsObsPage] =
      Iso[Observation.Id, TargetsObsPage](TargetsObsPage.apply)(_.obsId)
  }

  object ConstraintsObsPage {
    final val obsId: Iso[Observation.Id, ConstraintsObsPage] =
      Iso[Observation.Id, ConstraintsObsPage](ConstraintsObsPage.apply)(_.obsId)
  }
}
