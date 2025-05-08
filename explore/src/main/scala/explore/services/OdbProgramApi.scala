// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ProgramInfo
import explore.model.ProgramNote
import explore.model.ProgramUser
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.Attachment
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB.Enums.*
import lucuma.schemas.ObservationDB.Types.*

trait OdbProgramApi[F[_]]:
  def createProgram(name:              Option[NonEmptyString]): F[ProgramInfo]
  def deleteProgram(id:                Program.Id): F[Unit]
  def undeleteProgram(id:              Program.Id): F[Unit]
  def updateProgram(input:             UpdateProgramsInput): F[Unit]
  def updateProgramName(id:            Program.Id, name:           Option[NonEmptyString]): F[Unit]
  def updateGoaShouldNotify(id:        Program.Id, shouldNotify:   Boolean): F[Unit]
  def updateAttachmentDescription(oid: Attachment.Id, desc:        Option[NonEmptyString]): F[Unit]
  def updateAttachmentChecked(oid:     Attachment.Id, checked:     Boolean): F[Unit]
  def updateProgramUsers(puid:         ProgramUser.Id, set:        ProgramUserPropertiesInput): F[Unit]
  def updateUserFallbackName(puid:     ProgramUser.Id, creditName: Option[String]): F[Unit]
  def updateUserFallbackEmail(puid:    ProgramUser.Id, email:      Option[String]): F[Unit]
  def updateProgramPartner(puid:       ProgramUser.Id, pl:         Option[PartnerLink]): F[Unit]
  def updateUserES(puid:               ProgramUser.Id, es:         Option[EducationalStatus]): F[Unit]
  def updateUserThesis(puid:           ProgramUser.Id, th:         Option[Boolean]): F[Unit]
  def updateUserHasDataAccess(puid:    ProgramUser.Id, hda:        Boolean): F[Unit]
  def updateUserGender(puid:           ProgramUser.Id, g:          Option[Gender]): F[Unit]
  def changeProgramUserRole(puid:      ProgramUser.Id, role:       ProgramUserRole): F[Unit]
  def updateConfigurationRequestStatus(
    rids:          List[ConfigurationRequest.Id],
    newStatus:     ConfigurationRequestStatus,
    justification: Option[NonEmptyString]
  ): F[Unit]
  def createProgramNote(programId:     Program.Id, title:          NonEmptyString): F[ProgramNote.Id]
  def deleteProgramNote(id:            ProgramNote.Id): F[Unit]
  def undeleteProgramNote(id:          ProgramNote.Id): F[Unit]
