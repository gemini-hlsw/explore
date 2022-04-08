// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import explore.model.Page
import explore.model.Page._
import explore.model.enum.AppTab
import explore.model.reusability._
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.ui.reusability._

final case class RoutingInfo(
  appTab:        AppTab,
  programId:     Program.Id,
  focusedObsSet: Option[ObsIdSet],
  focusedTarget: Option[Target.Id]
)

object RoutingInfo {
  implicit val reuseRoutingInfo: Reusability[RoutingInfo] = Reusability.derive

  // The only Page that doesn't have a program ID is the NoProgramPage, so instead of polluting RoutingInfo with
  // Option[Program.Id], we'll just associate a dummy id with it. NoProgramPage will need special handling, anyways.
  val dummyProgramId = Program.Id(Long.MaxValue: PosLong)

  def from(page: Page): RoutingInfo = page match {
    case NoProgramPage                         => RoutingInfo(AppTab.Overview, dummyProgramId, none, none)
    case HomePage(p)                           => RoutingInfo(AppTab.Overview, p, none, none)
    case ProposalPage(p)                       => RoutingInfo(AppTab.Proposal, p, none, none)
    case ObservationsBasePage(p)               => RoutingInfo(AppTab.Observations, p, none, none)
    case ObsPage(p, obsId)                     =>
      RoutingInfo(AppTab.Observations, p, ObsIdSet.one(obsId).some, none)
    case ObsTargetPage(p, obsId, targetId)     =>
      RoutingInfo(AppTab.Observations, p, ObsIdSet.one(obsId).some, targetId.some)
    case TargetsBasePage(p)                    => RoutingInfo(AppTab.Targets, p, none, none)
    case TargetsObsPage(p, obsId)              => RoutingInfo(AppTab.Targets, p, obsId.some, none)
    case TargetPage(p, targetId)               => RoutingInfo(AppTab.Targets, p, none, targetId.some)
    case TargetWithObsPage(p, obsId, targetId) =>
      RoutingInfo(AppTab.Targets, p, obsId.some, targetId.some)
    case ConfigurationsPage(p)                 => RoutingInfo(AppTab.Configurations, p, none, none)
    case ConstraintsBasePage(p)                => RoutingInfo(AppTab.Constraints, p, none, none)
    case ConstraintsObsPage(p, obsId)          => RoutingInfo(AppTab.Constraints, p, obsId.some, none)
  }

  def getPage(
    tab:           AppTab,
    programId:     Program.Id,
    focusedObsSet: Option[ObsIdSet],
    focusedTarget: Option[Target.Id]
  ): Page =
    tab match {
      case AppTab.Proposal       => ProposalPage(programId)
      case AppTab.Overview       => HomePage(programId)
      case AppTab.Observations   =>
        (focusedObsSet, focusedTarget) match {
          case (Some(obsIds), Some(targetId)) if obsIds.length === 1 =>
            ObsTargetPage(programId, obsIds.head, targetId)
          case (Some(obsIds), _) if obsIds.length === 1              => ObsPage(programId, obsIds.head)
          case _                                                     => ObservationsBasePage(programId)
        }
      case AppTab.Targets        =>
        (focusedObsSet, focusedTarget) match {
          case (Some(obsIds), Some(targetId)) => TargetWithObsPage(programId, obsIds, targetId)
          case (Some(obsIds), _)              => TargetsObsPage(programId, obsIds)
          case (_, Some(targetId))            => TargetPage(programId, targetId)
          case _                              => TargetsBasePage(programId)
        }
      case AppTab.Configurations => ConfigurationsPage(programId)
      case AppTab.Constraints    =>
        focusedObsSet
          .map(ConstraintsObsPage(programId, _))
          .getOrElse(ConstraintsBasePage(programId))
    }
}
