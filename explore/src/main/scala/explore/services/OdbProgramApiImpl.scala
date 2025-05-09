// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.implicits.*
import clue.FetchClient
import clue.data.syntax.*
import clue.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ProgramInfo
import explore.model.ProgramNote
import explore.model.ProgramUser
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.Attachment
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.ProgramQueriesGQL.*

trait OdbProgramApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends OdbProgramApi[F]:

  def createProgram(name: Option[NonEmptyString]): F[ProgramInfo] =
    CreateProgramMutation[F]
      .execute:
        CreateProgramInput(SET = ProgramPropertiesInput(name = name.orIgnore).assign)
      .raiseGraphQLErrors
      .map(_.createProgram.program)

  def deleteProgram(id: Program.Id): F[Unit] =
    UpdateProgramsMutation[F]
      .execute:
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          SET = ProgramPropertiesInput(existence = Existence.Deleted.assign)
        )
      .raiseGraphQLErrors
      .void

  def undeleteProgram(id: Program.Id): F[Unit] =
    UpdateProgramsMutation[F]
      .execute:
        UpdateProgramsInput(
          WHERE = id.toWhereProgram.assign,
          includeDeleted = true.assign,
          SET = ProgramPropertiesInput(existence = Existence.Present.assign)
        )
      .raiseGraphQLErrors
      .void

  def updateProgram(input: UpdateProgramsInput): F[Unit] =
    UpdateProgramsMutation[F].execute(input).raiseGraphQLErrors.void

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
      .raiseGraphQLErrors
      .void

  def updateAttachmentChecked(oid: Attachment.Id, checked: Boolean): F[Unit] =
    UpdateAttachmentMutation[F]
      .execute:
        UpdateAttachmentsInput(
          WHERE = WhereAttachment(id = WhereOrderAttachmentId(EQ = oid.assign).assign).assign,
          SET = AttachmentPropertiesInput(checked = checked.assign)
        )
      .raiseGraphQLErrors
      .void

  def updateProgramUsers(puid: ProgramUser.Id, set: ProgramUserPropertiesInput): F[Unit] =
    ProgramUsersMutation[F]
      .execute:
        UpdateProgramUsersInput(
          WHERE = puid.toWhereProgramUser.assign,
          SET = set
        )
      .raiseGraphQLErrors
      .void

  def updateUserFallbackName(puid: ProgramUser.Id, creditName: Option[String]): F[Unit] =
    val fallbackInput = UserProfileInput(creditName = creditName.orUnassign)
    val input         = ProgramUserPropertiesInput(fallbackProfile = fallbackInput.assign)
    updateProgramUsers(puid, input)

  def updateUserFallbackEmail(puid: ProgramUser.Id, email: Option[String]): F[Unit] =
    val fallbackInput = UserProfileInput(email = email.orUnassign)
    val input         = ProgramUserPropertiesInput(fallbackProfile = fallbackInput.assign)
    updateProgramUsers(puid, input)

  def updateProgramPartner(puid: ProgramUser.Id, pl: Option[PartnerLink]): F[Unit] =
    updateProgramUsers(puid, ProgramUserPropertiesInput(partnerLink = pl.toInput.assign))

  def updateUserES(puid: ProgramUser.Id, es: Option[EducationalStatus]): F[Unit] =
    updateProgramUsers(puid, ProgramUserPropertiesInput(educationalStatus = es.orUnassign))

  def updateUserThesis(puid: ProgramUser.Id, th: Option[Boolean]): F[Unit] =
    updateProgramUsers(puid, ProgramUserPropertiesInput(thesis = th.orUnassign))

  def updateUserHasDataAccess(puid: ProgramUser.Id, hda: Boolean): F[Unit] =
    updateProgramUsers(puid, ProgramUserPropertiesInput(hasDataAccess = hda.assign))

  def updateUserGender(puid: ProgramUser.Id, g: Option[Gender]): F[Unit] =
    updateProgramUsers(puid, ProgramUserPropertiesInput(gender = g.orUnassign))

  def changeProgramUserRole(puid: ProgramUser.Id, role: ProgramUserRole): F[Unit] =
    ChangeProgramUserRoleMutation[F]
      .execute:
        ChangeProgramUserRoleInput(programUserId = puid, newRole = role)
      .raiseGraphQLErrors
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
      .raiseGraphQLErrors
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
      .raiseGraphQLErrors
      .map(_.createProgramNote.programNote.id)

  def deleteProgramNote(id: ProgramNote.Id): F[Unit] =
    UpdateProgramNotesMutation[F]
      .execute:
        UpdateProgramNotesInput(
          WHERE = id.toWhereProgramNote.assign,
          SET = ProgramNotePropertiesInput(existence = Existence.Deleted.assign)
        )
      .raiseGraphQLErrors
      .void

  def undeleteProgramNote(id: ProgramNote.Id): F[Unit] =
    UpdateProgramNotesMutation[F]
      .execute:
        UpdateProgramNotesInput(
          WHERE = id.toWhereProgramNote.assign,
          includeDeleted = true.assign,
          SET = ProgramNotePropertiesInput(existence = Existence.Present.assign)
        )
      .raiseGraphQLErrors
      .void
