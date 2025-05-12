// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import explore.model.CallForProposal
import explore.model.Proposal
import lucuma.core.model.Program
import lucuma.core.model.ProposalReference
import lucuma.schemas.ObservationDB.Types.UpdateProposalInput
import lucuma.schemas.enums.ProposalStatus

trait OdbProposalApi[F[_]]:
  def openCfps(): F[List[CallForProposal]]
  def resolveProposalReference(proposalRef: ProposalReference): F[Option[Program.Id]]
  def createProposal(programId:             Program.Id, proposal:  Proposal): F[Unit]
  def updateProposal(input:                 UpdateProposalInput): F[Unit]
  def setProposalStatus(programId:          Program.Id, newStatus: ProposalStatus): F[Unit]
