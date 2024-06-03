// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import explore.model.Proposal
import explore.model.ProposalType
import explore.model.CallForProposalType
import lucuma.core.enums.ToOActivation
import lucuma.schemas.ObservationDB.Types.ClassicalInput
import lucuma.schemas.ObservationDB.Types.DemoScienceInput
import lucuma.schemas.ObservationDB.Types.DirectorsTimeInput
import lucuma.schemas.ObservationDB.Types.FastTurnaroundInput
import lucuma.schemas.ObservationDB.Types.LargeProgramInput
import lucuma.schemas.ObservationDB.Types.PoorWeatherInput
import lucuma.schemas.ObservationDB.Types.ProposalPropertiesInput
import lucuma.schemas.ObservationDB.Types.ProposalTypeInput
import lucuma.schemas.ObservationDB.Types.QueueInput
import lucuma.schemas.ObservationDB.Types.SystemVerificationInput
import lucuma.schemas.ObservationDB.Types.PartnerSplitInput
import lucuma.core.model.CallForProposals
import explore.model.PartnerSplit
import clue.data.Unassign

trait ProposalQueries:
  private def toOAUpdater(f: Endo[Input[ToOActivation]]) =
    ProposalTypeInput.demoScience.assign.andThen(DemoScienceInput.toOActivation).modify(f) >>>
      ProposalTypeInput.directorsTime.assign.andThen(DirectorsTimeInput.toOActivation).modify(f) >>>
      ProposalTypeInput.fastTurnaround.assign
        .andThen(FastTurnaroundInput.toOActivation)
        .modify(f) >>>
      ProposalTypeInput.largeProgram.assign.andThen(LargeProgramInput.toOActivation).modify(f) >>>
      ProposalTypeInput.queue.assign.andThen(QueueInput.toOActivation).modify(f) >>>
      ProposalTypeInput.systemVerification.assign
        .andThen(SystemVerificationInput.toOActivation)
        .modify(f)

  def modifyToOActivation(f: Endo[Input[ToOActivation]]): Endo[ProposalPropertiesInput] =
    ProposalPropertiesInput.`type`.modify(_.map(toOAUpdater(f)))

  def modifyPartnerSplits(f: Endo[Input[List[PartnerSplitInput]]]): Endo[ProposalPropertiesInput] =
    ???
    // ProposalPropertiesInput.`type`.modify(_.map(toOAUpdater(f)))

  extension (proposalType: ProposalType)
    def toInput: ProposalTypeInput =
      proposalType match
        case ProposalType.DemoScience(_, toOActivation, minPercentTime)                   =>
          ProposalTypeInput(demoScience =
            DemoScienceInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            ).assign
          )
        case ProposalType.DirectorsTime(_, toOActivation, minPercentTime)                 =>
          ProposalTypeInput(directorsTime =
            DirectorsTimeInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            ).assign
          )
        case ProposalType.FastTurnaround(_, toOActivation, minPercentTime, piAffiliation) =>
          ProposalTypeInput(fastTurnaround =
            FastTurnaroundInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              piAffiliation = piAffiliation.orUnassign
            ).assign
          )
        case ProposalType.LargeProgram(_,
                                       toOActivation,
                                       minPercentTime,
                                       minPercentTotalTime,
                                       totalTime
            ) =>
          ProposalTypeInput(largeProgram =
            LargeProgramInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              minPercentTotalTime = minPercentTotalTime.orUnassign
              // totalTime = totalTime.map((r: TimeSpan) => r.toInput).orUnassign
            ).assign
          )
        case ProposalType.Classical(_, minPercentTime, partnerSplits)                     =>
          ProposalTypeInput(classical =
            ClassicalInput(
              minPercentTime = minPercentTime.assign,
              partnerSplits =
                if (partnerSplits.nonEmpty) partnerSplits.map(_.toInput).assign else Unassign
            ).assign
          )
        case ProposalType.Queue(_, toOActivation, minPercentTime, partnerSplits)          =>
          println(toOActivation)
          ProposalTypeInput(queue =
            QueueInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              partnerSplits =
                if (partnerSplits.nonEmpty) partnerSplits.map(_.toInput).assign else Unassign
            ).assign
          )
        case ProposalType.SystemVerification(_, toOActivation, minPercentTime)            =>
          ProposalTypeInput(systemVerification =
            SystemVerificationInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            ).assign
          )
        case ProposalType.PoorWeather(scienceSubtype)                                     =>
          ProposalTypeInput(poorWeather = PoorWeatherInput().assign)

  // Used to reset the proposal type when the call changes
  extension (cfpType: CallForProposalType)
    def defaultType: ProposalType = cfpType match
      case CallForProposalType.DemoScience        => ProposalType.DemoScience.Default
      case CallForProposalType.DirectorsTime      => ProposalType.DirectorsTime.Default
      case CallForProposalType.FastTurnaround     => ProposalType.FastTurnaround.Default
      case CallForProposalType.LargeProgram       => ProposalType.LargeProgram.Default
      case CallForProposalType.PoorWeather        => ProposalType.PoorWeather.Default
      case CallForProposalType.RegularSemester    => ProposalType.Queue.Default
      case CallForProposalType.SystemVerification => ProposalType.SystemVerification.Default

  extension (split: PartnerSplit)
    def toInput: PartnerSplitInput =
      PartnerSplitInput(partner = split.partner, percent = split.percent)

  extension (proposal: Proposal)
    def toInput: ProposalPropertiesInput =
      ProposalPropertiesInput(
        callId = proposal.callId.orUnassign,
        title = proposal.title.orUnassign,
        category = proposal.category.orUnassign,
        `abstract` = proposal.abstrakt.orUnassign,
        `type` = proposal.proposalType.map(_.toInput).orUnassign
      )

object ProposalQueries extends ProposalQueries
