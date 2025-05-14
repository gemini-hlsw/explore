// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Resource
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
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*
import queries.common.InvitationQueriesGQL.CreateInviteMutation.Data.CreateUserInvitation

trait OdbProgramApi[F[_]]:
  def createProgram(name:                    Option[NonEmptyString]): F[ProgramInfo]
  def deleteProgram(id:                      Program.Id): F[Unit]
  def undeleteProgram(id:                    Program.Id): F[Unit]
  def updateProgram(input:                   UpdateProgramsInput): F[Unit]
  def updateProgramName(id:                  Program.Id, name:           Option[NonEmptyString]): F[Unit]
  def updateGoaShouldNotify(id:              Program.Id, shouldNotify:   Boolean): F[Unit]
  def updateAttachmentDescription(oid:       Attachment.Id, desc:        Option[NonEmptyString]): F[Unit]
  def updateAttachmentChecked(oid:           Attachment.Id, checked:     Boolean): F[Unit]
  def addProgramUser(programId:              Program.Id, role:           ProgramUserRole): F[ProgramUser]
  def deleteProgramUser(programUserId:       ProgramUser.Id): F[Unit]
  def updateProgramUsers(programUserId:      ProgramUser.Id, set:        ProgramUserPropertiesInput): F[Unit]
  def updateUserFallbackName(programUserId:  ProgramUser.Id, creditName: Option[String]): F[Unit]
  def updateUserFallbackEmail(programUserId: ProgramUser.Id, email:      Option[String]): F[Unit]
  def updateProgramPartner(programUserId:    ProgramUser.Id, pl:         Option[PartnerLink]): F[Unit]
  def updateUserES(programUserId:            ProgramUser.Id, es:         Option[EducationalStatus]): F[Unit]
  def updateUserThesis(programUserId:        ProgramUser.Id, th:         Option[Boolean]): F[Unit]
  def updateUserHasDataAccess(programUserId: ProgramUser.Id, hda:        Boolean): F[Unit]
  def updateUserGender(programUserId:        ProgramUser.Id, g:          Option[Gender]): F[Unit]
  def changeProgramUserRole(programUserId:   ProgramUser.Id, role:       ProgramUserRole): F[Unit]
  def updateConfigurationRequestStatus(
    rids:          List[ConfigurationRequest.Id],
    newStatus:     ConfigurationRequestStatus,
    justification: Option[NonEmptyString]
  ): F[Unit]
  def createProgramNote(programId:           Program.Id, title:          NonEmptyString): F[ProgramNote.Id]
  def deleteProgramNote(id:                  ProgramNote.Id): F[Unit]
  def undeleteProgramNote(id:                ProgramNote.Id): F[Unit]
  def updateProgramNote(input:               UpdateProgramNotesInput): F[Unit]
  def resolveProgramReference(programRef:    ProgramReference): F[Option[Program.Id]]
  def createUserInvitation(
    programUserId: ProgramUser.Id,
    email:         EmailAddress
  ): F[CreateUserInvitation]
  def revokeUserInvitation(userInvitationId: String): F[Unit]
  def redeemUserInvitation(key:              String): F[RedeemInvitationResult]
  def programTimes(programId:                Program.Id): F[Option[ProgramTimes]]
  def programDetails(programId:              Program.Id): F[Option[ProgramDetails]]
  def allPrograms: F[List[ProgramInfo]]
  def allProgramAttachments(programId:       Program.Id): F[List[Attachment]]
  def programEditsSubscription(programId:    Program.Id): Resource[F, fs2.Stream[F, ProgramDetails]]
  def programAttachmentsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, List[Attachment]]]
  def programDeltaSubscription(programId:    Program.Id): Resource[F, fs2.Stream[F, ProgramInfo]]
