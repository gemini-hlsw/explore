// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.Page.*
import explore.model.enums.AppTab
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Program
import lucuma.refined.*
import monocle.Focus
import monocle.Lens

case class RoutingInfo(appTab: AppTab, optProgramId: Option[Program.Id], focused: Focused)
    derives Eq {
  // The only Page that doesn't have a program ID is the NoProgramPage, so instead of forcing everyplace to deal
  // Option[Program.Id], we'll just associate a dummy id with it. NoProgramPage will need special handling, anyways.
  def programId: Program.Id = optProgramId.getOrElse(RoutingInfo.dummyProgramId)
}

object RoutingInfo {
  var appTab: Lens[RoutingInfo, AppTab]                   = Focus[RoutingInfo](_.appTab)
  var optProgramId: Lens[RoutingInfo, Option[Program.Id]] = Focus[RoutingInfo](_.optProgramId)
  var focused: Lens[RoutingInfo, Focused]                 = Focus[RoutingInfo](_.focused)

  // The only Page that doesn't have a program ID is the NoProgramPage, so instead of polluting RoutingInfo with
  // Option[Program.Id], we'll just associate a dummy id with it. NoProgramPage will need special handling, anyways.
  private val dummyProgramId = Program.Id(Long.MaxValue.refined)

  private val fromPage: PartialFunction[Page, RoutingInfo] =
    case NoProgramPage                         => RoutingInfo(AppTab.Overview, none, Focused.None)
    case ProgramPage(p)                        => RoutingInfo(AppTab.Program, p.some, Focused.None)
    case HomePage(p)                           => RoutingInfo(AppTab.Overview, p.some, Focused.None)
    case ProposalPage(p)                       => RoutingInfo(AppTab.Proposal, p.some, Focused.None)
    case ObservationsBasePage(p)               => RoutingInfo(AppTab.Observations, p.some, Focused.None)
    case ObsPage(p, obsId)                     => RoutingInfo(AppTab.Observations, p.some, Focused.singleObs(obsId))
    case ObsGroupPage(p, groupId)              =>
      RoutingInfo(AppTab.Observations, p.some, Focused.group(groupId))
    case ObsTargetPage(p, obsId, targetId)     =>
      RoutingInfo(AppTab.Observations, p.some, Focused.singleObs(obsId, targetId.some))
    case TargetsBasePage(p)                    => RoutingInfo(AppTab.Targets, p.some, Focused.None)
    case TargetsObsPage(p, obsIds)             => RoutingInfo(AppTab.Targets, p.some, Focused.obsSet(obsIds))
    case TargetPage(p, targetId)               => RoutingInfo(AppTab.Targets, p.some, Focused.target(targetId))
    case TargetWithObsPage(p, obsId, targetId) =>
      RoutingInfo(AppTab.Targets, p.some, Focused(obsId.some, targetId.some))
    // case ConfigurationsPage(p)                 => RoutingInfo(AppTab.Configurations, p.some, Focused.None)
    case ConstraintsBasePage(p)                => RoutingInfo(AppTab.Constraints, p.some, Focused.None)
    case ConstraintsObsPage(p, obsIds)         =>
      RoutingInfo(AppTab.Constraints, p.some, Focused.obsSet(obsIds))
    case SchedulingBasePage(p)                 => RoutingInfo(AppTab.Scheduling, p.some, Focused.None)
    case SchedulingObsPage(p, obsIds)          =>
      RoutingInfo(AppTab.Scheduling, p.some, Focused.obsSet(obsIds))

  val from: Page => Option[RoutingInfo] = fromPage.lift

  def getPage(location: Option[(AppTab, Program.Id, Focused)]): Page =
    location
      .map: (tab, programId, focused) =>
        tab match
          case AppTab.Program      => ProgramPage(programId)
          case AppTab.Proposal     => ProposalPage(programId)
          case AppTab.Overview     => HomePage(programId)
          case AppTab.Observations =>
            focused match
              case Focused(Some(obsIds), Some(targetId), _) if obsIds.length === 1 =>
                ObsTargetPage(programId, obsIds.head, targetId)
              case Focused(Some(obsIds), _, _) if obsIds.length === 1              =>
                ObsPage(programId, obsIds.head)
              case Focused(_, _, Some(groupId))                                    =>
                ObsGroupPage(programId, groupId)
              case _                                                               =>
                ObservationsBasePage(programId)
          case AppTab.Targets      =>
            focused match {
              case Focused(Some(obsIds), Some(targetId), _) =>
                TargetWithObsPage(programId, obsIds, targetId)
              case Focused(Some(obsIds), _, _)              =>
                TargetsObsPage(programId, obsIds)
              case Focused(_, Some(targetId), _)            =>
                TargetPage(programId, targetId)
              case _                                        =>
                TargetsBasePage(programId)
            }
          // case AppTab.Configurations => ConfigurationsPage(programId)
          case AppTab.Constraints  =>
            focused.obsSet
              .map(ConstraintsObsPage(programId, _))
              .getOrElse(ConstraintsBasePage(programId))
          case AppTab.Scheduling   =>
            focused.obsSet
              .map(SchedulingObsPage(programId, _))
              .getOrElse(SchedulingBasePage(programId))
      .getOrElse(NoProgramPage)

  given Reusability[RoutingInfo] = Reusability.byEq
}
