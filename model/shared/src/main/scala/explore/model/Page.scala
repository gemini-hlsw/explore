// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import cats.derived.*
import cats.syntax.all._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import monocle.Iso

sealed trait Page extends Product with Serializable derives Eq

object Page:
  case object NoProgramPage                                             extends Page
  case class HomePage(programId: Program.Id)                            extends Page
  case class ProposalPage(programId: Program.Id)                        extends Page
  case class ObservationsBasePage(programId: Program.Id)                extends Page
  case class ObsPage(programId: Program.Id, obsId: Observation.Id)      extends Page
  case class ObsTargetPage(programId: Program.Id, obsId: Observation.Id, targetId: Target.Id)
      extends Page
  case class TargetsBasePage(programId: Program.Id)                     extends Page
  case class TargetsObsPage(programId: Program.Id, obsId: ObsIdSet)     extends Page
  case class TargetPage(programId: Program.Id, targetId: Target.Id)     extends Page
  case class TargetWithObsPage(programId: Program.Id, obsId: ObsIdSet, targetId: Target.Id)
      extends Page
  // case class ConfigurationsPage(programId: Program.Id)                  extends Page
  case class ConstraintsBasePage(programId: Program.Id)                 extends Page
  case class ConstraintsObsPage(programId: Program.Id, obsId: ObsIdSet) extends Page
  case class SchedulingBasePage(programId: Program.Id)                  extends Page
  case class SchedulingObsPage(programId: Program.Id, obsId: ObsIdSet)  extends Page

  object HomePage:
    final val iso: Iso[Program.Id, HomePage] =
      Iso[Program.Id, HomePage](HomePage.apply)(_.programId)

  object ProposalPage:
    final val iso: Iso[Program.Id, ProposalPage] =
      Iso[Program.Id, ProposalPage](ProposalPage.apply)(_.programId)

  object ObservationsBasePage:
    final val iso: Iso[Program.Id, ObservationsBasePage] =
      Iso[Program.Id, ObservationsBasePage](ObservationsBasePage.apply)(_.programId)

  object ObsPage:
    final val iso: Iso[(Program.Id, Observation.Id), ObsPage] =
      Iso[(Program.Id, Observation.Id), ObsPage] { case (p, o) => ObsPage(p, o) }(p =>
        (p.programId, p.obsId)
      )

  object ObsTargetPage:
    final val iso: Iso[(Program.Id, Observation.Id, Target.Id), ObsTargetPage] =
      Iso[(Program.Id, Observation.Id, Target.Id), ObsTargetPage] { case (p, o, t) =>
        ObsTargetPage(p, o, t)
      }(p => (p.programId, p.obsId, p.targetId))

  object TargetsBasePage:
    final val iso: Iso[Program.Id, TargetsBasePage] =
      Iso[Program.Id, TargetsBasePage](TargetsBasePage.apply)(_.programId)

  object TargetsObsPage:
    final val iso: Iso[(Program.Id, NonEmptySet[Observation.Id]), TargetsObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id]), TargetsObsPage] { case (p, o) =>
        TargetsObsPage(p, ObsIdSet(o))
      }(p => (p.programId, p.obsId.idSet))

  object TargetPage:
    final val iso: Iso[(Program.Id, Target.Id), TargetPage] =
      Iso[(Program.Id, Target.Id), TargetPage] { case (p, t) => TargetPage(p, t) }(p =>
        (p.programId, p.targetId)
      )

  object TargetWithObsPage:
    final val iso: Iso[(Program.Id, NonEmptySet[Observation.Id], Target.Id), TargetWithObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id], Target.Id), TargetWithObsPage] {
        case (p, o, t) =>
          TargetWithObsPage(p, ObsIdSet(o), t)
      }(p => (p.programId, p.obsId.idSet, p.targetId))

  // object ConfigurationsPage:
  //   final val iso: Iso[Program.Id, ConfigurationsPage] =
  //     Iso[Program.Id, ConfigurationsPage](ConfigurationsPage.apply)(_.programId)

  object ConstraintsBasePage:
    final val iso: Iso[Program.Id, ConstraintsBasePage] =
      Iso[Program.Id, ConstraintsBasePage](ConstraintsBasePage.apply)(_.programId)

  object ConstraintsObsPage:
    final val iso: Iso[(Program.Id, NonEmptySet[Observation.Id]), ConstraintsObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id]), ConstraintsObsPage] { case (p, o) =>
        ConstraintsObsPage(p, ObsIdSet(o))
      }(p => (p.programId, p.obsId.idSet))

  object SchedulingBasePage:
    final val iso: Iso[Program.Id, SchedulingBasePage] =
      Iso[Program.Id, SchedulingBasePage](SchedulingBasePage.apply)(_.programId)

  object SchedulingObsPage:
    final val iso: Iso[(Program.Id, NonEmptySet[Observation.Id]), SchedulingObsPage] =
      Iso[(Program.Id, NonEmptySet[Observation.Id]), SchedulingObsPage] { case (p, o) =>
        SchedulingObsPage(p, ObsIdSet(o))
      }(p => (p.programId, p.obsId.idSet))
