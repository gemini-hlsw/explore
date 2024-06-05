// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.data.Input
import clue.data.Unassign
import clue.data.syntax.*
import explore.model.CallForProposalType
import explore.model.PartnerSplit
import explore.model.Proposal
import explore.model.ProposalType
import lucuma.core.enums.ToOActivation
import lucuma.core.model.CallForProposals
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Types.ClassicalInput
import lucuma.schemas.ObservationDB.Types.DemoScienceInput
import lucuma.schemas.ObservationDB.Types.DirectorsTimeInput
import lucuma.schemas.ObservationDB.Types.FastTurnaroundInput
import lucuma.schemas.ObservationDB.Types.LargeProgramInput
import lucuma.schemas.ObservationDB.Types.PartnerSplitInput
import lucuma.schemas.ObservationDB.Types.PoorWeatherInput
import lucuma.schemas.ObservationDB.Types.ProposalPropertiesInput
import lucuma.schemas.ObservationDB.Types.ProposalTypeInput
import lucuma.schemas.ObservationDB.Types.QueueInput
import lucuma.schemas.ObservationDB.Types.SystemVerificationInput
import lucuma.schemas.ObservationDB.Types.TimeSpanInput

trait ProposalQueries:
  // This is on import lucuma.schemas.odb.input.* but it is not picked up for some reason
  extension (ts: TimeSpan)
    def toInput: TimeSpanInput = TimeSpanInput(microseconds = ts.toMicroseconds.assign)

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
              minPercentTotalTime = minPercentTotalTime.assign,
              totalTime = totalTime.toInput.assign
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
