// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.services.OdbProposalApi
import explore.syntax.ui.*
import explore.utils.ToastCtx
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.util.NewBoolean
import lucuma.core.util.Timestamp
import lucuma.core.util.time.format.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.Tag
import lucuma.react.primereact.Toolbar
import lucuma.schemas.enums.ProposalStatus
import lucuma.ui.format.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import org.typelevel.log4cats.Logger

import java.time.Duration
import java.time.LocalDateTime
import scala.concurrent.duration.*

def deadlineString(deadline: Timestamp): String = {
  val deadlineLDT = deadline.toLocalDateTime
  s"${GppDateFormatter.format(deadlineLDT)} ${GppTimeTZFormatterWithZone.format(deadlineLDT)}"
}

def deadlineAndTimeLeft(now: Timestamp, deadline: Timestamp): (String, Option[String]) = {
  val deadlineLDT: LocalDateTime = deadline.toLocalDateTime
  val nowLDT: LocalDateTime      = now.toLocalDateTime
  val diff: Duration             = Duration.between(nowLDT, deadlineLDT)
  val deadlineStr: String        = deadlineString(deadline)
  if (diff.isNegative) (deadlineStr, None)
  else
    val left = DurationLongWithSecondsFormatter(diff)
    (deadlineStr, left.some)
}

case class ProposalSubmissionBar(
  programId:         Program.Id,
  proposalStatus:    View[ProposalStatus],
  deadline:          Option[Either[String, Timestamp]],
  canSubmit:         Boolean,
  hasProposalErrors: Boolean
) extends ReactFnProps(ProposalSubmissionBar)

object ProposalSubmissionBar
    extends ReactFnComponent[ProposalSubmissionBar](props =>
      object IsUpdatingStatus extends NewBoolean
      type IsUpdatingStatus = IsUpdatingStatus.Type

      def doUpdateStatus(
        programId:              Program.Id,
        isUpdatingStatus:       View[IsUpdatingStatus],
        setLocalProposalStatus: ProposalStatus => IO[Unit]
      )(
        newStatus:              ProposalStatus
      )(using odbApi: OdbProposalApi[IO])(using Logger[IO], ToastCtx[IO]): Callback =
        odbApi
          .setProposalStatus(programId, newStatus)
          .flatMap(_ => setLocalProposalStatus(newStatus))
          .toastErrors
          .switching(isUpdatingStatus.async, IsUpdatingStatus(_))
          .runAsync

      for {
        ctx              <- useContext(AppContext.ctx)
        isUpdatingStatus <- useStateView(IsUpdatingStatus(false))
        errorMessage     <-
          useMemo((props.proposalStatus.get, props.hasProposalErrors)): (ps, hasErrors) =>
            if (hasErrors && ps === ProposalStatus.NotSubmitted)
              "Proposal cannot be submitted with errors. See errors tile for details.".some
            else none
        nowPot           <- useStreamOnMount:
                              Stream
                                .fixedRateStartImmediately[IO](1.second)
                                .evalMap: _ =>
                                  IO.realTime.map: finiteDuration =>
                                    Timestamp.ofEpochMilli(finiteDuration.toMillis)
      } yield
        import ctx.given

        val updateStatus: ProposalStatus => Callback =
          doUpdateStatus(
            props.programId,
            isUpdatingStatus,
            props.proposalStatus.async.set
          )

        nowPot.toOption.flatten.map: now =>
          val isDueDeadline: Boolean = props.deadline.flatMap(_.toOption).forall(_ < now)

          Toolbar(left =
            <.div(ExploreStyles.ProposalSubmissionBar)(
              Tag(
                value = props.proposalStatus.get.name,
                severity =
                  if (props.proposalStatus.get === ProposalStatus.Accepted) Tag.Severity.Success
                  else Tag.Severity.Danger
              )
                .when(props.proposalStatus.get > ProposalStatus.Submitted),
              React
                .Fragment(
                  Button(
                    label = "Submit Proposal",
                    onClick = updateStatus(ProposalStatus.Submitted),
                    disabled =
                      isUpdatingStatus.get.value || props.hasProposalErrors || isDueDeadline || !props.canSubmit
                    // Temporarily enable submission even if there are errors for testing against API validation
                    // isUpdatingStatus.get.value || isDueDeadline || !props.canSubmit
                  ).compact.tiny,
                  props.deadline.map: deadlineEither =>
                    val (text, severity) = deadlineEither match
                      case Right(deadline) =>
                        val (deadlineStr, left): (String, Option[String]) =
                          deadlineAndTimeLeft(now, deadline)
                        val text: String                                  =
                          left.fold(deadlineStr)(l => s"$deadlineStr [$l]")
                        val severity: Message.Severity                    =
                          left.fold(Message.Severity.Error)(_ => Message.Severity.Info)
                        s"Deadline: $text" -> severity
                      case Left(error)     =>
                        error -> Message.Severity.Info

                    <.span(ExploreStyles.ProposalDeadline)(
                      Message(
                        text = text,
                        severity = severity
                      )
                    )
                )
                .when:
                  props.proposalStatus.get === ProposalStatus.NotSubmitted
              ,
              Button(
                "Retract Proposal",
                severity = Button.Severity.Warning,
                onClick = updateStatus(ProposalStatus.NotSubmitted),
                disabled = isUpdatingStatus.get.value || isDueDeadline || !props.canSubmit
              ).compact.tiny
                .when:
                  props.proposalStatus.get === ProposalStatus.Submitted && !isDueDeadline
              ,
              errorMessage.value
                .map(r =>
                  <.span(ExploreStyles.ProposalDeadline)(
                    Message(text = r, severity = Message.Severity.Error)
                  )
                )
            )
          )
    )
