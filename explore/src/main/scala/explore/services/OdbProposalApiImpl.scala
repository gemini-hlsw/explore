// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import clue.model.GraphQLResponse.*
import explore.common.ProposalOdbExtensions.*
import explore.model.CallForProposal
import explore.model.Proposal
import lucuma.core.model.Program
import lucuma.core.model.ProposalReference
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.CreateProposalInput
import lucuma.schemas.ObservationDB.Types.SetProposalStatusInput
import lucuma.schemas.ObservationDB.Types.UpdateProposalInput
import lucuma.schemas.enums.ProposalStatus
import queries.common.CallsQueriesGQL.*
import queries.common.ProgramQueriesGQL.ResolveProposalReference
import queries.common.ProposalQueriesGQL.*

trait OdbProposalApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends OdbProposalApi[F]:
  self: OdbApiHelper[F] =>

  def openCfps(): F[List[CallForProposal]] =
    ReadOpenCFPs[F]
      .query()
      .processErrors
      .map(_.callsForProposals.matches)

  def resolveProposalReference(proposalRef: ProposalReference): F[Option[Program.Id]] =
    ResolveProposalReference[F]
      .query(proposalRef.assign)
      .processErrors
      .map(_.program.map(_.id))

  def createProposal(programId: Program.Id, proposal: Proposal): F[Unit] =
    CreateProposalMutation[F]
      .execute:
        CreateProposalInput(programId = programId, SET = proposal.toInput)
      .processErrors
      .void

  def updateProposal(input: UpdateProposalInput): F[Unit] =
    UpdateProposalMutation[F].execute(input).processErrors.void

  def setProposalStatus(programId: Program.Id, newStatus: ProposalStatus): F[Unit] =
    SetProposalStatus[F]
      .execute:
        SetProposalStatusInput(programId = programId.assign, status = newStatus)
      .raiseGraphQLErrorsOnNoData
      .void
