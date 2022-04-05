// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import explore.model.Page
import explore.model.Page._
import explore.model.enum.AppTab
import explore.model.reusability._
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Target
import lucuma.ui.reusability._

final case class RoutingInfo(
  appTab:        AppTab,
  focusedObsSet: Option[ObsIdSet],
  focusedTarget: Option[Target.Id]
)

object RoutingInfo {
  implicit val reuseRoutingInfo: Reusability[RoutingInfo] = Reusability.derive

  def from(page: Page): RoutingInfo = page match {
    case HomePage                           => RoutingInfo(AppTab.Overview, none, none)
    case ProposalPage                       => RoutingInfo(AppTab.Proposal, none, none)
    case ObservationsBasePage               => RoutingInfo(AppTab.Observations, none, none)
    case ObsPage(obsId)                     => RoutingInfo(AppTab.Observations, ObsIdSet.one(obsId).some, none)
    case ObsTargetPage(obsId, targetId)     =>
      RoutingInfo(AppTab.Observations, ObsIdSet.one(obsId).some, targetId.some)
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
    focusedObsSet: Option[ObsIdSet],
    focusedTarget: Option[Target.Id]
  ): Page =
    tab match {
      case AppTab.Proposal       => ProposalPage
      case AppTab.Overview       => HomePage
      case AppTab.Observations   =>
        (focusedObsSet, focusedTarget) match {
          case (Some(obsIds), Some(targetId)) if obsIds.length === 1 =>
            ObsTargetPage(obsIds.head, targetId)
          case (Some(obsIds), _) if obsIds.length === 1              => ObsPage(obsIds.head)
          case _                                                     => ObservationsBasePage
        }
      case AppTab.Targets        =>
        (focusedObsSet, focusedTarget) match {
          case (Some(obsIds), Some(targetId)) => TargetWithObsPage(obsIds, targetId)
          case (Some(obsIds), _)              => TargetsObsPage(obsIds)
          case (_, Some(targetId))            => TargetPage(targetId)
          case _                              => TargetsBasePage
        }
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    =>
        focusedObsSet.map(ConstraintsObsPage(_)).getOrElse(ConstraintsBasePage)
    }
}
