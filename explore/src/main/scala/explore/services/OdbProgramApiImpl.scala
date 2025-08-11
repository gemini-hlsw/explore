// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.effect.Resource
import cats.implicits.*
import clue.StreamingClient
import clue.data.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Attachment
import explore.model.ProgramDetails
import explore.model.ProgramInfo
import explore.model.ProgramNote
import explore.model.ProgramTimes
import explore.model.ProgramUser
import explore.model.RedeemInvitationResult
import lucuma.core.data.EmailAddress
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.InvitationQueriesGQL.*
import queries.common.InvitationQueriesGQL.CreateInviteMutation.Data.CreateUserInvitation
import queries.common.ProgramQueriesGQL.*
import queries.common.ProgramSummaryQueriesGQL.AllProgramAttachments
import queries.common.ProgramSummaryQueriesGQL.AllPrograms
import queries.common.ProgramSummaryQueriesGQL.ProgramDetailsQuery
import queries.common.ProgramSummaryQueriesGQL.ProgramTimesQuery
import queries.common.ProposalQueriesGQL.AddProgramUser
import queries.common.ProposalQueriesGQL.DeleteProgramUser

trait OdbProgramApiImpl[F[_]: MonadThrow](using StreamingClient[F, ObservationDB])
    extends OdbProgramApi[F]:
  self: OdbApiHelper[F] =>

  def createProgram(name: Option[NonEmptyString]): F[ProgramInfo] =
    CreateProgramMutation[F]
      .execute:
        CreateProgramInput(SET = ProgramPropertiesInput(name = name.orIgnore).assign)
      .processErrors
      .map(_.createProgram.program)

  def deleteProgram(id: Program.Id): F[Unit] =
    UpdateProgramsMutation[F]
      .execute:
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(existence = Existence.Deleted.assign)
        )
      .processErrors
      .void

  def undeleteProgram(id: Program.Id): F[Unit] =
    UpdateProgramsMutation[F]
      .execute:
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          includeDeleted = true.assign,
          SET = ProgramPropertiesInput(existence = Existence.Present.assign)
        )
      .processErrors
      .void

  def updateProgram(input: UpdateProgramsInput): F[Unit] =
    UpdateProgramsMutation[F].execute(input).processErrors.void

  def updateProgramName(id: Program.Id, name: Option[NonEmptyString]): F[Unit] =
    updateProgram:
      UpdateProgramsInput(
        WHERE = id.toWhereProgram.assign,
        SET = ProgramPropertiesInput(name = name.orUnassign)
      )

  def updateGoaShouldNotify(id: Program.Id, shouldNotify: Boolean): F[Unit] =
    updateProgram:
      UpdateProgramsInput(
        WHERE = id.toWhereProgram.assign,
        SET = ProgramPropertiesInput(
          goa = GoaPropertiesInput(shouldNotify = shouldNotify.assign).assign
        )
      )

  def updateAttachmentDescription(oid: Attachment.Id, desc: Option[NonEmptyString]): F[Unit] =
    UpdateAttachmentMutation[F]
      .execute:
        UpdateAttachmentsInput(
          WHERE = WhereAttachment(id = WhereOrderAttachmentId(EQ = oid.assign).assign).assign,
          SET = AttachmentPropertiesInput(description = desc.orUnassign)
        )
      .processErrors
      .void

  def updateAttachmentChecked(oid: Attachment.Id, checked: Boolean): F[Unit] =
    UpdateAttachmentMutation[F]
      .execute:
        UpdateAttachmentsInput(
          WHERE = WhereAttachment(id = WhereOrderAttachmentId(EQ = oid.assign).assign).assign,
          SET = AttachmentPropertiesInput(checked = checked.assign)
        )
      .processErrors
      .void

  def addProgramUser(programId: Program.Id, role: ProgramUserRole): F[ProgramUser] =
    val input = AddProgramUserInput(programId = programId, role = role)
    AddProgramUser[F]
      .execute(input)
      .processErrors
      .map(_.addProgramUser.programUser)

  def deleteProgramUser(programUserId: ProgramUser.Id): F[Unit] =
    DeleteProgramUser[F].execute(programUserId.toDeleteInput).processErrors.void

  def updateProgramUsers(programUserId: ProgramUser.Id, set: ProgramUserPropertiesInput): F[Unit] =
    ProgramUsersMutation[F]
      .execute:
        UpdateProgramUsersInput(
          WHERE = programUserId.toWhereProgramUser.assign,
          SET = set
        )
      .processErrors
      .void

  def updateUserPreferredName(programUserId: ProgramUser.Id, creditName: Option[String]): F[Unit] =
    val preferredInput = UserProfileInput(creditName = creditName.orUnassign)
    val input          = ProgramUserPropertiesInput(preferredProfile = preferredInput.assign)
    updateProgramUsers(programUserId, input)

  def updateUserPreferredEmail(programUserId: ProgramUser.Id, email: Option[String]): F[Unit] =
    val preferredInput = UserProfileInput(email = email.orUnassign)
    val input          = ProgramUserPropertiesInput(preferredProfile = preferredInput.assign)
    updateProgramUsers(programUserId, input)

  def updateUserPartner(programUserId: ProgramUser.Id, pl: PartnerLink): F[Unit] =
    updateProgramUsers(programUserId, ProgramUserPropertiesInput(partnerLink = pl.toInput.assign))

  def updateUserES(programUserId: ProgramUser.Id, es: Option[EducationalStatus]): F[Unit] =
    updateProgramUsers(programUserId, ProgramUserPropertiesInput(educationalStatus = es.orUnassign))

  def updateUserThesis(programUserId: ProgramUser.Id, th: Option[Boolean]): F[Unit] =
    updateProgramUsers(programUserId, ProgramUserPropertiesInput(thesis = th.orUnassign))

  def updateUserHasDataAccess(programUserId: ProgramUser.Id, hda: Boolean): F[Unit] =
    updateProgramUsers(programUserId, ProgramUserPropertiesInput(hasDataAccess = hda.assign))

  def updateUserGender(programUserId: ProgramUser.Id, g: Option[Gender]): F[Unit] =
    updateProgramUsers(programUserId, ProgramUserPropertiesInput(gender = g.orUnassign))

  def updateUserAffiliation(uid: ProgramUser.Id, aff: Option[NonEmptyString]): F[Unit] =
    updateProgramUsers(uid, ProgramUserPropertiesInput(affiliation = aff.orUnassign))

  def changeProgramUserRole(programUserId: ProgramUser.Id, role: ProgramUserRole): F[Unit] =
    ChangeProgramUserRoleMutation[F]
      .execute:
        ChangeProgramUserRoleInput(programUserId = programUserId, newRole = role)
      .processErrors
      .void

  // Note: If justification is none, it is ignored, not un-set. We
  // (currently, at least) do not allow unsetting justifications in explore.
  def updateConfigurationRequestStatus(
    rids:          List[ConfigurationRequest.Id],
    newStatus:     ConfigurationRequestStatus,
    justification: Option[NonEmptyString]
  ): F[Unit] =
    UpdateConfigurationRequestsMutation[F]
      .execute:
        UpdateConfigurationRequestsInput(
          WHERE = rids.toWhereConfigurationRequest.assign,
          SET = ConfigurationRequestProperties(
            status = newStatus.assign,
            justification = justification.orIgnore
          )
        )
      .processErrors
      .void

  def createProgramNote(programId: Program.Id, title: NonEmptyString): F[ProgramNote.Id] =
    CreateProgramNoteMutation[F]
      .execute:
        CreateProgramNoteInput(
          programId = programId.assign,
          SET = ProgramNotePropertiesInput(
            title = title.assign
          )
        )
      .processErrors
      .map(_.createProgramNote.programNote.id)

  def deleteProgramNote(id: ProgramNote.Id): F[Unit] =
    UpdateProgramNotesMutation[F]
      .execute:
        UpdateProgramNotesInput(
          WHERE = id.toWhereProgramNote.assign,
          SET = ProgramNotePropertiesInput(existence = Existence.Deleted.assign)
        )
      .processErrors
      .void

  def undeleteProgramNote(id: ProgramNote.Id): F[Unit] =
    UpdateProgramNotesMutation[F]
      .execute:
        UpdateProgramNotesInput(
          WHERE = id.toWhereProgramNote.assign,
          includeDeleted = true.assign,
          SET = ProgramNotePropertiesInput(existence = Existence.Present.assign)
        )
      .processErrors
      .void

  def updateProgramNote(input: UpdateProgramNotesInput): F[Unit] =
    UpdateProgramNotesMutation[F].execute(input).processErrors.void

  def resolveProgramReference(programRef: ProgramReference): F[Option[Program.Id]] =
    ResolveProgramReference[F]
      .query(programRef.assign)
      .processErrors
      .map(_.program.map(_.id))

  def createUserInvitation(
    programUserId: ProgramUser.Id,
    email:         EmailAddress
  ): F[CreateUserInvitation] =
    CreateInviteMutation[F]
      .execute(programUserId, email.value.value)
      .processErrors
      .map(_.createUserInvitation)

  def revokeUserInvitation(userInvitationId: String): F[Unit] =
    RevokeInvitationMutation[F].execute(userInvitationId).processErrors.void

  def redeemUserInvitation(key: String): F[RedeemInvitationResult] =
    RedeemInvitationMutation[F]
      .execute(key)
      .processErrors
      .map(_.redeemUserInvitation.invitation)

  def programTimes(programId: Program.Id): F[Option[ProgramTimes]] =
    ProgramTimesQuery[F]
      .query(programId)
      .processErrors
      .map(_.program)

  def programDetails(programId: Program.Id): F[Option[ProgramDetails]] =
    ProgramDetailsQuery[F]
      .query(programId)
      .processErrors
      .map(_.program)

  val allPrograms: F[List[ProgramInfo]] =
    drain[ProgramInfo, Program.Id, AllPrograms.Data.Programs](
      offset =>
        AllPrograms[F]
          .query(offset.orUnassign)
          .processErrors
          .map(_.programs),
      _.matches,
      _.hasMore,
      _.id
    )

  def allProgramAttachments(programId: Program.Id): F[List[Attachment]] =
    AllProgramAttachments[F]
      .query(programId)
      .processErrors
      .map:
        _.program.fold(List.empty)(_.attachments)

  def programEditsSubscription(programId: Program.Id): Resource[F, fs2.Stream[F, ProgramDetails]] =
    ProgramEditDetailsSubscription
      .subscribe[F](programId.toProgramEditInput)
      .processErrors("ProgramEditDetailsSubscription")
      .map(_.map(_.programEdit.value))

  def programAttachmentsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, List[Attachment]]] =
    ProgramEditAttachmentSubscription
      .subscribe[F](programId.toProgramEditInput)
      .processErrors("ProgramEditAttachmentSubscription")
      .map(_.map(_.programEdit.value.attachments))

  def programDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ProgramInfo]] =
    ProgramInfoDelta
      .subscribe[F]()
      .processErrors("ProgramInfoDelta")
      .map(_.map(_.programEdit.value))
