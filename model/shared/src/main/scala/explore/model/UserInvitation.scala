// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import io.circe.*
import io.circe.refined.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.InvitationStatus
import lucuma.core.util.Enumerated

case class UserInvitation(
  id:          String,
  email:       EmailAddress,
  status:      InvitationStatus,
  emailStatus: Option[EmailStatus]
) derives Eq:
  val isRevoked: Boolean = status === InvitationStatus.Revoked

  lazy val deliveryStatus: UserInvitation.DeliveryStatus =
    import UserInvitation.*
    import UserInvitation.DeliveryStatus.*
    status match
      case InvitationStatus.Pending => emailStatus.deliveryStatus

      case InvitationStatus.Declined => Failed("The invitee has declined the invitation.")
      // In the current implementation, we will never be asked for these 2
      case InvitationStatus.Redeemed => Success("The invitation has been redeemed.")
      case InvitationStatus.Revoked  => Failed("The invitation has been revoked.")

object UserInvitation:
  sealed abstract class DeliveryStatus(val message: String)
  object DeliveryStatus:
    case class Success(override val message: String)    extends DeliveryStatus(message)
    case class InProgress(override val message: String) extends DeliveryStatus(message)
    case class Failed(override val message: String)     extends DeliveryStatus(message)

  extension (optEmailStatus: Option[EmailStatus])
    def deliveryStatus: DeliveryStatus =
      import DeliveryStatus.*

      optEmailStatus match
        case None              => InProgress("Email has not yet been sent.")
        case Some(emailStatus) =>
          val msg = emailStatus.description
          emailStatus match
            case EmailStatus.Queued           => InProgress(msg)
            case EmailStatus.Rejected         => Failed(msg)
            case EmailStatus.Accepted         => InProgress(msg)
            case EmailStatus.Delivered        => Success(msg)
            case EmailStatus.PermanentFailure => Failed(msg)
            case EmailStatus.TemporaryFailure => Failed(msg)

  given Decoder[UserInvitation] = c =>
    for {
      id <- c.get[String]("id")
      em <- c.get[EmailAddress]("recipientEmail")
      s  <- c.get[InvitationStatus]("status")
      es <- c.downField("email").downField("status").success.traverse(_.as[EmailStatus])
    } yield UserInvitation(id, em, s, es)
