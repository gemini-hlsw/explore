// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import explore.model.Page
import explore.model.Page._
import explore.model.enum.AppTab
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.ui.reusability._

final case class RoutingInfo(
  appTab:        AppTab,
  focusedObs:    Option[Observation.Id],
  focusedTarget: Option[Target.Id]
)

object RoutingInfo {
  implicit val reuseRoutingInfo: Reusability[RoutingInfo] = Reusability.derive

  def from(page: Page): RoutingInfo = page match {
    case HomePage                           => RoutingInfo(AppTab.Overview, none, none)
    case ProposalPage                       => RoutingInfo(AppTab.Proposal, none, none)
    case ObservationsBasePage               => RoutingInfo(AppTab.Observations, none, none)
    case ObsPage(obsId)                     => RoutingInfo(AppTab.Observations, obsId.some, none)
    case ObsTargetPage(obsId, targetId)     =>
      RoutingInfo(AppTab.Observations, obsId.some, targetId.some)
    case TargetsBasePage                    => RoutingInfo(AppTab.Targets, none, none)
    case TargetsObsPage(obsId)              => RoutingInfo(AppTab.Targets, obsId.some, none)
    case TargetPage(targetId)               => RoutingInfo(AppTab.Targets, none, targetId.some)
    case TargetWithObsPage(obsId, targetId) =>
      RoutingInfo(AppTab.Targets, obsId.some, targetId.some)
    case ConfigurationsPage                 => RoutingInfo(AppTab.Configurations, none, none)
    case ConstraintsBasePage                => RoutingInfo(AppTab.Constraints, none, none)
    case ConstraintsObsPage(obsId)          => RoutingInfo(AppTab.Constraints, obsId.some, none)
  }

  def getPage(
    tab:           AppTab,
    focusedObs:    Option[Observation.Id],
    focusedTarget: Option[Target.Id]
  ): Page =
    tab match {
      case AppTab.Proposal       => ProposalPage
      case AppTab.Overview       => HomePage
      case AppTab.Observations   =>
        (focusedObs, focusedTarget) match {
          case (Some(obsId), Some(targetId)) => ObsTargetPage(obsId, targetId)
          case (Some(obsId), _)              => ObsPage(obsId)
          case _                             => ObservationsBasePage
        }
      case AppTab.Targets        =>
        (focusedObs, focusedTarget) match {
          case (Some(obsId), Some(targetId)) => TargetWithObsPage(obsId, targetId)
          case (Some(obsId), _)              => TargetsObsPage(obsId)
          case (_, Some(targetId))           => TargetPage(targetId)
          case _                             => TargetsBasePage
        }
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    =>
        focusedObs.map(ConstraintsObsPage(_)).getOrElse(ConstraintsBasePage)
    }
}
