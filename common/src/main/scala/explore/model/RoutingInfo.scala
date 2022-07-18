// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import explore.model.Page
import explore.model.Page._
import explore.model.enums.AppTab
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Program
import lucuma.ui.reusability._

final case class RoutingInfo(appTab: AppTab, optProgramId: Option[Program.Id], focused: Focused) {
  // The only Page that doesn't have a program ID is the NoProgramPage, so instead of forcing everyplace to deal
  // Option[Program.Id], we'll just associate a dummy id with it. NoProgramPage will need special handling, anyways.
  def programId: Program.Id = optProgramId.getOrElse(RoutingInfo.dummyProgramId)
}

object RoutingInfo {
  implicit val reuseRoutingInfo: Reusability[RoutingInfo] = Reusability.derive

  // The only Page that doesn't have a program ID is the NoProgramPage, so instead of polluting RoutingInfo with
  // Option[Program.Id], we'll just associate a dummy id with it. NoProgramPage will need special handling, anyways.
  val dummyProgramId = Program.Id(Long.MaxValue: PosLong)

  def from(page: Page): RoutingInfo = page match {
    case NoProgramPage                         => RoutingInfo(AppTab.Overview, none, Focused.None)
    case HomePage(p)                           => RoutingInfo(AppTab.Overview, p.some, Focused.None)
    case ProposalPage(p)                       => RoutingInfo(AppTab.Proposal, p.some, Focused.None)
    case ObservationsBasePage(p)               => RoutingInfo(AppTab.Observations, p.some, Focused.None)
    case ObsPage(p, obsId)                     => RoutingInfo(AppTab.Observations, p.some, Focused.singleObs(obsId))
    case ObsTargetPage(p, obsId, targetId)     =>
      RoutingInfo(AppTab.Observations, p.some, Focused.singleObs(obsId, targetId.some))
    case TargetsBasePage(p)                    => RoutingInfo(AppTab.Targets, p.some, Focused.None)
    case TargetsObsPage(p, obsIds)             => RoutingInfo(AppTab.Targets, p.some, Focused.obsSet(obsIds))
    case TargetPage(p, targetId)               => RoutingInfo(AppTab.Targets, p.some, Focused.target(targetId))
    case TargetWithObsPage(p, obsId, targetId) =>
      RoutingInfo(AppTab.Targets, p.some, Focused(obsId.some, targetId.some))
    case ConfigurationsPage(p)                 => RoutingInfo(AppTab.Configurations, p.some, Focused.None)
    case ConstraintsBasePage(p)                => RoutingInfo(AppTab.Constraints, p.some, Focused.None)
    case ConstraintsObsPage(p, obsIds)         =>
      RoutingInfo(AppTab.Constraints, p.some, Focused.obsSet(obsIds))
  }

  def getPage(
    tab:       AppTab,
    programId: Program.Id,
    focused:   Focused
  ): Page =
    tab match {
      case AppTab.Proposal       => ProposalPage(programId)
      case AppTab.Overview       => HomePage(programId)
      case AppTab.Observations   =>
        focused match {
          case Focused(Some(obsIds), Some(targetId)) if obsIds.length === 1 =>
            ObsTargetPage(programId, obsIds.head, targetId)
          case Focused(Some(obsIds), _) if obsIds.length === 1              => ObsPage(programId, obsIds.head)
          case _                                                            => ObservationsBasePage(programId)
        }
      case AppTab.Targets        =>
        focused match {
          case Focused(Some(obsIds), Some(targetId)) =>
            TargetWithObsPage(programId, obsIds, targetId)
          case Focused(Some(obsIds), _)              => TargetsObsPage(programId, obsIds)
          case Focused(_, Some(targetId))            => TargetPage(programId, targetId)
          case _                                     => TargetsBasePage(programId)
        }
      case AppTab.Configurations => ConfigurationsPage(programId)
      case AppTab.Constraints    =>
        focused.obsSet
          .map(ConstraintsObsPage(programId, _))
          .getOrElse(ConstraintsBasePage(programId))
    }
}
