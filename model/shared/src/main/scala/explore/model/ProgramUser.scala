// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.UserProfile
import lucuma.odb.json.partnerlink.given
import lucuma.sso.client.codec.userProfile.given
import monocle.Focus
import monocle.Lens

case class ProgramUser(
  id:                ProgramUser.Id,
  user:              Option[User],
  partnerLink:       Option[PartnerLink],
  role:              ProgramUserRole,
  educationalStatus: Option[EducationalStatus],
  thesis:            Option[Boolean],
  gender:            Option[Gender],
  affiliation:       Option[NonEmptyString],
  fallbackProfile:   UserProfile,
  invitations:       List[UserInvitation],
  hasDataAccess:     Boolean
) derives Eq:
  val name: String          =
    user.fold(fallbackProfile.displayName.orEmpty)(_.name)
  val email: Option[String] =
    user.flatMap(_.profile.flatMap(_.email)).orElse(fallbackProfile.email)
  // Try to get only a last name and fallback to display name. If there is a User, we'll
  // assume it has one of the names we can use.
  lazy val lastName: String =
    user.fold(fallbackProfile.familyName.orElse(fallbackProfile.displayName).orEmpty)(_.lastName)

  // should only ever be one non-revoked invitation
  val activeInvitation: Option[UserInvitation] =
    invitations.find(!_.isRevoked)

  lazy val status: ProgramUser.Status =
    user match
      case Some(_) => ProgramUser.Status.Confirmed
      case None    =>
        activeInvitation match
          case None      => ProgramUser.Status.NotInvited
          case Some(inv) => ProgramUser.Status.Invited(inv.deliveryStatus)

  lazy val isConfirmed: Boolean         = status === ProgramUser.Status.Confirmed
  lazy val successfullyInvited: Boolean = activeInvitation.exists(!_.deliveryStatus.failed)
  lazy val hasPhd: Boolean              = educationalStatus.exists(_ === EducationalStatus.PhD)

object ProgramUser:
  type Id = lucuma.core.model.ProgramUser.Id
  val Id = lucuma.core.model.ProgramUser.Id

  sealed abstract class Status(val message: String) derives Eq:
    val isInvited = this match
      case Status.Invited(_) => true
      case _                 => false

  object Status:
    object Confirmed                                                  extends Status("Confirmed")
    object NotInvited                                                 extends Status("Not Invited")
    case class Invited(deliveryStatus: UserInvitation.DeliveryStatus) extends Status("Invited")

  val id: Lens[ProgramUser, Id] = Focus[ProgramUser](_.id)

  val user: Lens[ProgramUser, Option[User]] = Focus[ProgramUser](_.user)

  val partnerLink: Lens[ProgramUser, Option[PartnerLink]] =
    Focus[ProgramUser](_.partnerLink)

  val role: Lens[ProgramUser, ProgramUserRole] = Focus[ProgramUser](_.role)

  val educationalStatus: Lens[ProgramUser, Option[EducationalStatus]] =
    Focus[ProgramUser](_.educationalStatus)

  val thesis: Lens[ProgramUser, Option[Boolean]] =
    Focus[ProgramUser](_.thesis)

  val gender: Lens[ProgramUser, Option[Gender]] =
    Focus[ProgramUser](_.gender)

  val affiliation: Lens[ProgramUser, Option[NonEmptyString]] =
    Focus[ProgramUser](_.affiliation)

  val fallbackProfile: Lens[ProgramUser, UserProfile] =
    Focus[ProgramUser](_.fallbackProfile)

  val invitations: Lens[ProgramUser, List[UserInvitation]] =
    Focus[ProgramUser](_.invitations)

  val hasDataAccess: Lens[ProgramUser, Boolean] =
    Focus[ProgramUser](_.hasDataAccess)

  private val profileCreditNameNES: Lens[UserProfile, Option[NonEmptyString]] =
    Lens[UserProfile, Option[NonEmptyString]](
      _.creditName.flatMap(NonEmptyString.from(_).toOption)
    )(ones => UserProfile.creditName.replace(ones.map(_.value)))

  private val profileEmailAddress: Lens[UserProfile, Option[EmailAddress]] =
    Lens[UserProfile, Option[EmailAddress]](
      _.email.flatMap(EmailAddress.from(_).toOption)
    )(oe => UserProfile.email.replace(oe.map(_.value.value)))

  val fallbackCreditName: Lens[ProgramUser, Option[NonEmptyString]] =
    fallbackProfile.andThen(profileCreditNameNES)

  val fallbackEmail: Lens[ProgramUser, Option[EmailAddress]] =
    fallbackProfile.andThen(profileEmailAddress)

  given Decoder[ProgramUser] = c =>
    for {
      id   <- c.downField("id").as[Id]
      u    <- c.downField("user").as[Option[User]]
      pl   <- c.downField("partnerLink").as[Option[PartnerLink]]
      role <- c.downField("role").as[ProgramUserRole]
      es   <- c.downField("educationalStatus").as[Option[EducationalStatus]]
      th   <- c.downField("thesis").as[Option[Boolean]]
      g    <- c.downField("gender").as[Option[Gender]]
      aff  <- c.downField("affiliation").as[Option[NonEmptyString]]
      fb   <- c.downField("fallbackProfile").as[UserProfile]
      in   <- c.downField("invitations").as[List[UserInvitation]]
      da   <- c.downField("hasDataAccess").as[Boolean]
    } yield ProgramUser(id, u, pl, role, es, th, g, aff, fb, in, da)
