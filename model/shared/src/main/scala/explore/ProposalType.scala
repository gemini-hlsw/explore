// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.model.Partner
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import eu.timepit.refined.cats.given
import lucuma.core.util.TimeSpan
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.enums.ScienceSubtype
import lucuma.odb.json.time.decoder.given
import io.circe.ACursor
import monocle.Lens
import monocle.Focus
import monocle.Optional

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

  // Define the Classical case class implementing ProposalType
  case class Classical(
    scienceSubtype: ScienceSubtype,
    minPercentTime: IntPercent,
    partnerSplits:  List[PartnerSplit]
  ) extends ProposalType
      derives Eq

  object Classical {
    val minPercentTime: Lens[Classical, IntPercent] = Focus[Classical](_.minPercentTime)
  }

  // Define the DemoScience case class implementing ProposalType
  case class DemoScience(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType
      derives Eq

  object DemoScience {
    val minPercentTime: Lens[DemoScience, IntPercent]   = Focus[DemoScience](_.minPercentTime)
    val toOActivation: Lens[DemoScience, ToOActivation] = Focus[DemoScience](_.toOActivation)
  }

  // Define the DirectorsTime case class implementing ProposalType
  case class DirectorsTime(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType
      derives Eq

  object DirectorsTime {
    val minPercentTime: Lens[DirectorsTime, IntPercent]   = Focus[DirectorsTime](_.minPercentTime)
    val toOActivation: Lens[DirectorsTime, ToOActivation] = Focus[DirectorsTime](_.toOActivation)
  }

  // Define the FastTurnaround case class implementing ProposalType
  case class FastTurnaround(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent,
    piAffiliation:  Option[Partner]
  ) extends ProposalType
      derives Eq

  // Define the LargeProgram case class implementing ProposalType
  case class LargeProgram(
    scienceSubtype:      ScienceSubtype,
    toOActivation:       ToOActivation,
    minPercentTime:      IntPercent,
    minPercentTotalTime: IntPercent,
    totalTime:           TimeSpan
  ) extends ProposalType
      derives Eq

  // Define the PoorWeather case class implementing ProposalType
  case class PoorWeather(
    scienceSubtype: ScienceSubtype
  ) extends ProposalType
      derives Eq

  // Define the Queue case class implementing ProposalType
  case class Queue(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent,
    partnerSplits:  List[PartnerSplit]
  ) extends ProposalType
      derives Eq

  // Define the SystemVerification case class implementing ProposalType
  case class SystemVerification(
    scienceSubtype: ScienceSubtype,
    toOActivation:  ToOActivation,
    minPercentTime: IntPercent
  ) extends ProposalType

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
            piAffiliation  <- c.downField("piAffiliation").as[Option[Partner]]
          } yield FastTurnaround(tpe, toOActivation, minPercentTime, piAffiliation)
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
