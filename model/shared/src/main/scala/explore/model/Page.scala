// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Iso

sealed trait Page extends Product with Serializable

object Page {
  case object HomePage                   extends Page
  final case object ProposalPage         extends Page
  final case object ObservationsBasePage extends Page
  final case class ObsPage(obsId: Observation.Id) extends Page
  final case object TargetsBasePage      extends Page
  final case class TargetPage(targetId: Target.Id) extends Page
  final case class TargetsAsterismPage(asterismId: Asterism.Id) extends Page
  final case class TargetsObsPage(obsId: Observation.Id) extends Page
  case object ConfigurationsPage         extends Page
  case object ConstraintsPage            extends Page

  implicit val eqPage: Eq[Page] = Eq.instance {
    case (HomePage, HomePage)                             => true
    case (ProposalPage, ProposalPage)                     => true
    case (ObservationsBasePage, ObservationsBasePage)     => true
    case (ObsPage(a), ObsPage(b))                         => a === b
    case (TargetsBasePage, TargetsBasePage)               => true
    case (TargetPage(a), TargetPage(b))                   => a === b
    case (TargetsAsterismPage(a), TargetsAsterismPage(b)) => a === b
    case (TargetsObsPage(a), TargetsObsPage(b))           => a === b
    case (ConfigurationsPage, ConfigurationsPage)         => true
    case (ConstraintsPage, ConstraintsPage)               => true
    case _                                                => false
  }

  object ObsPage {
    final val obsId: Iso[Observation.Id, ObsPage] =
      Iso[Observation.Id, ObsPage](ObsPage.apply)(_.obsId)
  }

  object TargetPage {
    final val targetId: Iso[Target.Id, TargetPage] =
      Iso[Target.Id, TargetPage](TargetPage.apply)(_.targetId)
  }

  object TargetsAsterismPage {
    final val asterismId: Iso[Asterism.Id, TargetsAsterismPage] =
      Iso[Asterism.Id, TargetsAsterismPage](TargetsAsterismPage.apply)(_.asterismId)
  }

  object TargetsObsPage {
    final val obsId: Iso[Observation.Id, TargetsObsPage] =
      Iso[Observation.Id, TargetsObsPage](TargetsObsPage.apply)(_.obsId)
  }
}
