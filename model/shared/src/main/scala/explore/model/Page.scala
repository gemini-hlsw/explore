// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import cats.derived.*
import lucuma.core.model.Group
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.Target
import monocle.Iso

enum Page derives Eq:
  case NoProgramPage                                                                    extends Page
  case HomePage(programId: Program.Id)                                                  extends Page
  case ProgramPage(programId: Program.Id)                                               extends Page
  case ProposalPage(programId: Program.Id)                                              extends Page
  case ObservationsBasePage(programId: Program.Id)                                      extends Page
  case ObsPage(programId: Program.Id, obsId: Observation.Id)                            extends Page
  case ObsGroupPage(programId: Program.Id, groupId: Group.Id)                           extends Page
  case ObsTargetPage(programId: Program.Id, obsId: Observation.Id, targetId: Target.Id) extends Page
  case TargetsBasePage(programId: Program.Id)                                           extends Page
  case TargetsObsPage(programId: Program.Id, obsId: ObsIdSet)                           extends Page
  case TargetPage(programId: Program.Id, targetId: Target.Id)                           extends Page
  case TargetWithObsPage(programId: Program.Id, obsId: ObsIdSet, targetId: Target.Id)   extends Page
  // case  ConfigurationsPage(programId: Program.Id)                  extends Page
  case ConstraintsBasePage(programId: Program.Id)                                       extends Page
  case ConstraintsObsPage(programId: Program.Id, obsId: ObsIdSet)                       extends Page
  case SchedulingBasePage(programId: Program.Id)                                        extends Page
  case SchedulingObsPage(programId: Program.Id, obsId: ObsIdSet)                        extends Page
  case ProposalReferenceResolverPage(proposalRef: ProposalReference)                    extends Page
  case ProgramReferenceResolverPage(programRef: ProgramReference)                       extends Page
  case ObservationReferenceResolverPage(obsRef: ObservationReference)                   extends Page

object Page:
  object HomePage:
    val iso: Iso[Program.Id, HomePage] =
      Iso[Program.Id, HomePage](Page.HomePage(_))(_.programId)

  object ProgramPage:
    val iso: Iso[Program.Id, ProgramPage] =
      Iso[Program.Id, ProgramPage](Page.ProgramPage(_))(_.programId)

  object ProposalPage:
    val iso: Iso[Program.Id, ProposalPage] =
      Iso[Program.Id, ProposalPage](Page.ProposalPage(_))(_.programId)

  object ObservationsBasePage:
    val iso: Iso[Program.Id, ObservationsBasePage] =
      Iso[Program.Id, ObservationsBasePage](Page.ObservationsBasePage(_))(_.programId)

  object ObsPage:
    val iso: Iso[(Program.Id, Observation.Id), ObsPage] =
      Iso[(Program.Id, Observation.Id), ObsPage] { case (p, o) => Page.ObsPage(p, o) }(p =>
        (p.programId, p.obsId)
      )

  object ObsGroupPage:
    val iso: Iso[(Program.Id, Group.Id), ObsGroupPage] =
      Iso[(Program.Id, Group.Id), ObsGroupPage] { case (p, g) => Page.ObsGroupPage(p, g) }(p =>
        (p.programId, p.groupId)
      )

  object ObsTargetPage:
    val iso: Iso[(Program.Id, Observation.Id, Target.Id), ObsTargetPage] =
      Iso[(Program.Id, Observation.Id, Target.Id), ObsTargetPage] { case (p, o, t) =>
        Page.ObsTargetPage(p, o, t)
      }(p => (p.programId, p.obsId, p.targetId))

  object TargetsBasePage:
    val iso: Iso[Program.Id, TargetsBasePage] =
      Iso[Program.Id, TargetsBasePage](Page.TargetsBasePage(_))(_.programId)

  object TargetsObsPage:
    val iso: Iso[(Program.Id, NonEmptySet[Observation.Id]), TargetsObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id]), TargetsObsPage] { case (p, o) =>
        Page.TargetsObsPage(p, ObsIdSet(o))
      }(p => (p.programId, p.obsId.idSet))

  object TargetPage:
    val iso: Iso[(Program.Id, Target.Id), TargetPage] =
      Iso[(Program.Id, Target.Id), TargetPage] { case (p, t) => Page.TargetPage(p, t) }(p =>
        (p.programId, p.targetId)
      )

  object TargetWithObsPage:
    val iso: Iso[(Program.Id, NonEmptySet[Observation.Id], Target.Id), TargetWithObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id], Target.Id), TargetWithObsPage] {
        case (p, o, t) => Page.TargetWithObsPage(p, ObsIdSet(o), t)
      }(p => (p.programId, p.obsId.idSet, p.targetId))

  // object ConfigurationsPage:
  //   val iso: Iso[Program.Id, ConfigurationsPage] =
  //     Iso[Program.Id, ConfigurationsPage](Page.ConfigurationsPage(_))(_.programId)

  object ConstraintsBasePage:
    val iso: Iso[Program.Id, ConstraintsBasePage] =
      Iso[Program.Id, ConstraintsBasePage](Page.ConstraintsBasePage(_))(_.programId)

  object ConstraintsObsPage:
    val iso: Iso[(Program.Id, NonEmptySet[Observation.Id]), ConstraintsObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id]), ConstraintsObsPage] { case (p, o) =>
        Page.ConstraintsObsPage(p, ObsIdSet(o))
      }(p => (p.programId, p.obsId.idSet))

  object SchedulingBasePage:
    val iso: Iso[Program.Id, SchedulingBasePage] =
      Iso[Program.Id, SchedulingBasePage](Page.SchedulingBasePage(_))(_.programId)

  object SchedulingObsPage:
    val iso: Iso[(Program.Id, NonEmptySet[Observation.Id]), SchedulingObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id]), SchedulingObsPage] { case (p, o) =>
        Page.SchedulingObsPage(p, ObsIdSet(o))
      }(p => (p.programId, p.obsId.idSet))

  object ProposalReferenceResolverPage:
    val iso: Iso[ProposalReference, ProposalReferenceResolverPage] =
      Iso[ProposalReference, ProposalReferenceResolverPage](
        Page.ProposalReferenceResolverPage(_)
      )(_.proposalRef)

  object ProgramReferenceResolverPage:
    val iso: Iso[ProgramReference, ProgramReferenceResolverPage] =
      Iso[ProgramReference, ProgramReferenceResolverPage](
        Page.ProgramReferenceResolverPage(_)
      )(_.programRef)

  object ObservationReferenceResolverPage:
    val iso: Iso[ObservationReference, ObservationReferenceResolverPage] =
      Iso[ObservationReference, ObservationReferenceResolverPage](
        Page.ObservationReferenceResolverPage(_)
      )(_.obsRef)
