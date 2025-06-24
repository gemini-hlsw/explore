// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import io.circe.ACursor
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

// Define the ProposalType trait
sealed trait ProposalType derives Eq {
  val scienceSubtype: ScienceSubtype
}

object ProposalType:
  def toScienceSubtype(s: ScienceSubtype): ProposalType => ProposalType =
    s match
      case ScienceSubtype.Classical => {
        case Queue(_, _, minTime, splits) =>
          Classical(ScienceSubtype.Classical, minTime, splits)
        case i                            => i
      }
      case ScienceSubtype.Queue     => {
        case Classical(_, minTime, splits) =>
          Queue(ScienceSubtype.Queue, ToOActivation.None, minTime, splits)
        case i                             => i
      }
      case _                        => identity

  val toOActivation: Optional[ProposalType, ToOActivation] = Optional[ProposalType, ToOActivation] {
    case d: DemoScience        => d.toOActivation.some
    case d: DirectorsTime      => d.toOActivation.some
    case d: FastTurnaround     => d.toOActivation.some
    case d: LargeProgram       => d.toOActivation.some
    case d: Queue              => d.toOActivation.some
    case d: SystemVerification => d.toOActivation.some
    case _                     => none
  }(a => {
    case d: DemoScience        => d.copy(toOActivation = a)
    case d: DirectorsTime      => d.copy(toOActivation = a)
    case d: FastTurnaround     => d.copy(toOActivation = a)
    case d: LargeProgram       => d.copy(toOActivation = a)
    case d: Queue              => d.copy(toOActivation = a)
    case d: SystemVerification => d.copy(toOActivation = a)
    case i                     => i
  })

  val partnerSplits: Optional[ProposalType, List[PartnerSplit]] =
    Optional[ProposalType, List[PartnerSplit]] {
      case c: Classical => c.partnerSplits.some
      case q: Queue     => q.partnerSplits.some
      case _            => none
    }(a => {
      case c: Classical => c.copy(partnerSplits = a)
      case q: Queue     => q.copy(partnerSplits = a)
      case i            => i
    })

  val minPercentTime: Optional[ProposalType, IntPercent] =
    Optional[ProposalType, IntPercent] {
      case c: Classical          => c.minPercentTime.some
      case d: DemoScience        => d.minPercentTime.some
      case d: DirectorsTime      => d.minPercentTime.some
      case d: FastTurnaround     => d.minPercentTime.some
      case d: LargeProgram       => d.minPercentTime.some
      case d: Queue              => d.minPercentTime.some
      case d: SystemVerification => d.minPercentTime.some
      case _                     => none
    }(a => {
      case c: Classical          => c.copy(minPercentTime = a)
      case d: DemoScience        => d.copy(minPercentTime = a)
      case d: DirectorsTime      => d.copy(minPercentTime = a)
      case d: FastTurnaround     => d.copy(minPercentTime = a)
      case d: LargeProgram       => d.copy(minPercentTime = a)
      case d: Queue              => d.copy(minPercentTime = a)
      case d: SystemVerification => d.copy(minPercentTime = a)
      case i                     => i
    })

  val minPercentTotalTime: Optional[ProposalType, IntPercent] =
    Optional[ProposalType, IntPercent] {
      case l: LargeProgram => l.minPercentTotalTime.some
      case _               => none
    }(a => {
      case l: LargeProgram => l.copy(minPercentTotalTime = a)
      case i               => i
    })

  val totalTime: Optional[ProposalType, TimeSpan] =
    Optional[ProposalType, TimeSpan] {
      case l: LargeProgram => l.totalTime.some
      case _               => none
    }(a => {
      case l: LargeProgram => l.copy(totalTime = a)
      case i               => i
    })

  // Define the Classical case class implementing ProposalType
  case class Classical(
    scienceSubtype: ScienceSubtype,
    minPercentTime: IntPercent,
    partnerSplits:  List[PartnerSplit]
  ) extends ProposalType derives Eq

  object Classical {
    val minPercentTime: Lens[Classical, IntPercent] = Focus[Classical](_.minPercentTime)

    val Default: Classical = Classical(ScienceSubtype.Classical, 100.refined, List.empty)
  }

  // Define the DemoScience case class implementing ProposalType
  case class DemoScience(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType derives Eq

  object DemoScience {
    val minPercentTime: Lens[DemoScience, IntPercent]   = Focus[DemoScience](_.minPercentTime)
    val toOActivation: Lens[DemoScience, ToOActivation] = Focus[DemoScience](_.toOActivation)

    val Default: DemoScience =
      DemoScience(ScienceSubtype.DemoScience, ToOActivation.None, 100.refined)
  }

  // Define the DirectorsTime case class implementing ProposalType
  case class DirectorsTime(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType derives Eq

  object DirectorsTime {
    val minPercentTime: Lens[DirectorsTime, IntPercent]   = Focus[DirectorsTime](_.minPercentTime)
    val toOActivation: Lens[DirectorsTime, ToOActivation] = Focus[DirectorsTime](_.toOActivation)

    val Default: DirectorsTime =
      DirectorsTime(ScienceSubtype.DirectorsTime, ToOActivation.None, 100.refined)
  }

  // Define the FastTurnaround case class implementing ProposalType
  case class FastTurnaround(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent,
    reviewerId:     Option[ProgramUser.Id],
    mentorId:       Option[ProgramUser.Id]
  ) extends ProposalType derives Eq

  object FastTurnaround {
    val minPercentTime: Lens[FastTurnaround, IntPercent]         = Focus[FastTurnaround](_.minPercentTime)
    val toOActivation: Lens[FastTurnaround, ToOActivation]       = Focus[FastTurnaround](_.toOActivation)
    val reviewerId: Lens[FastTurnaround, Option[ProgramUser.Id]] =
      Focus[FastTurnaround](_.reviewerId)
    val mentorId: Lens[FastTurnaround, Option[ProgramUser.Id]]   = Focus[FastTurnaround](_.mentorId)

    val Default: FastTurnaround =
      FastTurnaround(ScienceSubtype.FastTurnaround, ToOActivation.None, 100.refined, None, None)

    def defaultWithReviewer(id: Option[ProgramUser.Id]): FastTurnaround =
      reviewerId.replace(id)(Default)
  }

  // Define the LargeProgram case class implementing ProposalType
  case class LargeProgram(
    scienceSubtype:      ScienceSubtype,
    toOActivation:       ToOActivation,
    minPercentTime:      IntPercent,
    minPercentTotalTime: IntPercent,
    totalTime:           TimeSpan
  ) extends ProposalType derives Eq

  object LargeProgram {
    val minPercentTime: Lens[LargeProgram, IntPercent]      = Focus[LargeProgram](_.minPercentTime)
    val minPercentTotalTime: Lens[LargeProgram, IntPercent] =
      Focus[LargeProgram](_.minPercentTotalTime)
    val toOActivation: Lens[LargeProgram, ToOActivation]    = Focus[LargeProgram](_.toOActivation)
    val totalTime: Lens[LargeProgram, TimeSpan]             = Focus[LargeProgram](_.totalTime)

    val Default: LargeProgram =
      LargeProgram(ScienceSubtype.LargeProgram,
                   ToOActivation.None,
                   100.refined,
                   100.refined,
                   TimeSpan.Zero
      )
  }

  // Define the PoorWeather case class implementing ProposalType
  case class PoorWeather(
    scienceSubtype: ScienceSubtype
  ) extends ProposalType derives Eq

  object PoorWeather {
    val Default: PoorWeather = PoorWeather(ScienceSubtype.PoorWeather)
  }

  // Define the Queue case class implementing ProposalType
  case class Queue(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent,
    partnerSplits:  List[PartnerSplit]
  ) extends ProposalType derives Eq

  object Queue {
    val minPercentTime: Lens[Queue, IntPercent]   = Focus[Queue](_.minPercentTime)
    val toOActivation: Lens[Queue, ToOActivation] = Focus[Queue](_.toOActivation)

    val Default: Queue = Queue(ScienceSubtype.Queue, ToOActivation.None, 100.refined, List.empty)
  }

  // Define the SystemVerification case class implementing ProposalType
  case class SystemVerification(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType

  object SystemVerification {
    val minPercentTime: Lens[SystemVerification, IntPercent]   =
      Focus[SystemVerification](_.minPercentTime)
    val toOActivation: Lens[SystemVerification, ToOActivation] =
      Focus[SystemVerification](_.toOActivation)

    val Default: SystemVerification =
      SystemVerification(ScienceSubtype.SystemVerification, ToOActivation.None, 100.refined)
  }

  val classical: Prism[ProposalType, Classical]                   = GenPrism[ProposalType, Classical]
  val directorsTime: Prism[ProposalType, DirectorsTime]           = GenPrism[ProposalType, DirectorsTime]
  val demoScience: Prism[ProposalType, DemoScience]               = GenPrism[ProposalType, DemoScience]
  val fastTurnaround: Prism[ProposalType, FastTurnaround]         = GenPrism[ProposalType, FastTurnaround]
  val largeProgram: Prism[ProposalType, LargeProgram]             = GenPrism[ProposalType, LargeProgram]
  val poorWeather: Prism[ProposalType, PoorWeather]               = GenPrism[ProposalType, PoorWeather]
  val queue: Prism[ProposalType, Queue]                           = GenPrism[ProposalType, Queue]
  val systemVerification: Prism[ProposalType, SystemVerification] =
    GenPrism[ProposalType, SystemVerification]

  given Decoder[ProposalType] = {

    def toProposalType(tpe: ScienceSubtype, c: ACursor): Decoder.Result[ProposalType] =
      tpe match
        case ScienceSubtype.Classical          =>
          for {
            minPercentTime <- c.downField("minPercentTime").as[IntPercent]
            partnerSplits  <- c.downField("partnerSplits").as[List[PartnerSplit]]
          } yield Classical(tpe, minPercentTime, partnerSplits)
        case ScienceSubtype.DemoScience        =>
          for {
            toOActivation  <- c.downField("toOActivation").as[ToOActivation]
            minPercentTime <- c.downField("minPercentTime").as[IntPercent]
          } yield DemoScience(tpe, toOActivation, minPercentTime)
        case ScienceSubtype.DirectorsTime      =>
          for {
            toOActivation  <- c.downField("toOActivation").as[ToOActivation]
            minPercentTime <- c.downField("minPercentTime").as[IntPercent]
          } yield DirectorsTime(tpe, toOActivation, minPercentTime)
        case ScienceSubtype.FastTurnaround     =>
          for {
            toOActivation  <- c.downField("toOActivation").as[ToOActivation]
            minPercentTime <- c.downField("minPercentTime").as[IntPercent]
            reviewerId     <-
              c.downField("reviewer").downField("id").success.traverse(_.as[Option[ProgramUser.Id]])
            mentorId       <-
              c.downField("mentor").downField("id").success.traverse(_.as[Option[ProgramUser.Id]])
          } yield FastTurnaround(tpe,
                                 toOActivation,
                                 minPercentTime,
                                 reviewerId.flatten,
                                 mentorId.flatten
          )
        case ScienceSubtype.LargeProgram       =>
          for {
            toOActivation       <- c.downField("toOActivation").as[ToOActivation]
            minPercentTime      <- c.downField("minPercentTime").as[IntPercent]
            minPercentTotalTime <- c.downField("minPercentTotalTime").as[IntPercent]
            totalTime           <- c.downField("totalTime").as[TimeSpan]
          } yield LargeProgram(tpe, toOActivation, minPercentTime, minPercentTotalTime, totalTime)
        case ScienceSubtype.PoorWeather        =>
          Right(PoorWeather(tpe))
        case ScienceSubtype.Queue              =>
          for {
            toOActivation  <- c.downField("toOActivation").as[ToOActivation]
            minPercentTime <- c.downField("minPercentTime").as[IntPercent]
            partnerSplits  <- c.downField("partnerSplits").as[List[PartnerSplit]]
          } yield Queue(tpe, toOActivation, minPercentTime, partnerSplits)
        case ScienceSubtype.SystemVerification =>
          for {
            toOActivation  <- c.downField("toOActivation").as[ToOActivation]
            minPercentTime <- c.downField("minPercentTime").as[IntPercent]
          } yield SystemVerification(tpe, toOActivation, minPercentTime)

    Decoder.instance { c =>
      for {
        tpe <- c.downField("scienceSubtype").as[ScienceSubtype]
        pt  <- toProposalType(tpe, c)
      } yield pt
    }
  }
