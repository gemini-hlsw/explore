// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import lucuma.core.enums.Partner
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import explore.model.arb.ArbPartnerSplit.given
import explore.model.ProposalType
import explore.model.ProposalType.*
import explore.model.PartnerSplit
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.core.enums.ScienceSubtype

trait ArbProposalType:

  given Arbitrary[Classical] =
    Arbitrary {
      for {
        scienceSubtype <- arbitrary[ScienceSubtype]
        minPercentType <- arbitrary[IntPercent]
        partnerSplits  <- arbitrary[List[PartnerSplit]]
      } yield Classical(scienceSubtype, minPercentType, partnerSplits)
    }

  given Cogen[Classical] =
    Cogen[
      (
        ScienceSubtype,
        IntPercent,
        List[PartnerSplit]
      )
    ].contramap(p => (p.scienceSubtype, p.minPercentTime, p.partnerSplits))

  given Arbitrary[DemoScience] =
    Arbitrary {
      for {
        scienceSubtype <- arbitrary[ScienceSubtype]
        toOActivation  <- arbitrary[ToOActivation]
        minPercentType <- arbitrary[IntPercent]
      } yield DemoScience(scienceSubtype, toOActivation, minPercentType)
    }

  given Cogen[DemoScience] =
    Cogen[
      (
        ScienceSubtype,
        ToOActivation,
        IntPercent
      )
    ].contramap(p => (p.scienceSubtype, p.toOActivation, p.minPercentTime))

  given Arbitrary[DirectorsTime] =
    Arbitrary {
      for {
        scienceSubtype <- arbitrary[ScienceSubtype]
        toOActivation  <- arbitrary[ToOActivation]
        minPercentType <- arbitrary[IntPercent]
      } yield DirectorsTime(scienceSubtype, toOActivation, minPercentType)
    }

  given Cogen[DirectorsTime] =
    Cogen[
      (
        ScienceSubtype,
        ToOActivation,
        IntPercent
      )
    ].contramap(p => (p.scienceSubtype, p.toOActivation, p.minPercentTime))

  given Arbitrary[FastTurnaround] =
    Arbitrary {
      for {
        scienceSubtype <- arbitrary[ScienceSubtype]
        toOActivation  <- arbitrary[ToOActivation]
        minPercentType <- arbitrary[IntPercent]
        piAffiliation  <- arbitrary[Option[Partner]]
      } yield FastTurnaround(scienceSubtype, toOActivation, minPercentType, piAffiliation)
    }

  given Cogen[FastTurnaround] =
    Cogen[
      (
        ScienceSubtype,
        ToOActivation,
        IntPercent,
        Option[Partner]
      )
    ].contramap(p => (p.scienceSubtype, p.toOActivation, p.minPercentTime, p.piAffiliation))

  given Arbitrary[LargeProgram] =
    Arbitrary {
      for {
        scienceSubtype      <- arbitrary[ScienceSubtype]
        toOActivation       <- arbitrary[ToOActivation]
        minPercentType      <- arbitrary[IntPercent]
        minPercentTotalTime <- arbitrary[IntPercent]
        totalTime           <- arbitrary[TimeSpan]
      } yield LargeProgram(scienceSubtype,
                           toOActivation,
                           minPercentType,
                           minPercentTotalTime,
                           totalTime
      )
    }

  given Cogen[LargeProgram] = Cogen[
    (
      ScienceSubtype,
      ToOActivation,
      IntPercent,
      IntPercent,
      TimeSpan
    )
  ].contramap(p =>
    (p.scienceSubtype, p.toOActivation, p.minPercentTime, p.minPercentTotalTime, p.totalTime)
  )

  given Arbitrary[PoorWeather] =
    Arbitrary {
      for {
        scienceSubtype <- arbitrary[ScienceSubtype]
      } yield PoorWeather(scienceSubtype)
    }

  given Cogen[PoorWeather] = Cogen[
    ScienceSubtype
  ].contramap(p => p.scienceSubtype)

  given Arbitrary[Queue] =
    Arbitrary {
      for {
        scienceSubtype <- arbitrary[ScienceSubtype]
        toOActivation  <- arbitrary[ToOActivation]
        minPercentType <- arbitrary[IntPercent]
        partnerSplits  <- arbitrary[List[PartnerSplit]]
      } yield Queue(scienceSubtype, toOActivation, minPercentType, partnerSplits)
    }

  given Cogen[Queue] =
    Cogen[
      (
        ScienceSubtype,
        ToOActivation,
        IntPercent,
        List[PartnerSplit]
      )
    ].contramap(p => (p.scienceSubtype, p.toOActivation, p.minPercentTime, p.partnerSplits))

  given Arbitrary[SystemVerification] =
    Arbitrary {
      for {
        scienceSubtype <- arbitrary[ScienceSubtype]
        toOActivation  <- arbitrary[ToOActivation]
        minPercentTime <- arbitrary[IntPercent]
      } yield SystemVerification(scienceSubtype, toOActivation, minPercentTime)
    }

  given Cogen[SystemVerification] =
    Cogen[
      (
        ScienceSubtype,
        ToOActivation,
        IntPercent
      )
    ].contramap(p => (p.scienceSubtype, p.toOActivation, p.minPercentTime))

  given Arbitrary[ProposalType] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[Classical],
        arbitrary[DemoScience],
        arbitrary[DirectorsTime],
        arbitrary[FastTurnaround],
        arbitrary[LargeProgram],
        arbitrary[PoorWeather],
        arbitrary[Queue],
        arbitrary[SystemVerification]
      )
    }

  given Cogen[ProposalType] =
    Cogen[Either[Classical, Either[DemoScience, Either[DirectorsTime, Either[
      FastTurnaround,
      Either[LargeProgram, Either[PoorWeather, Either[Queue, SystemVerification]]]
    ]]]]].contramap {
      case c: Classical          => Left(c)
      case d: DemoScience        => Right(Left(d))
      case d: DirectorsTime      => Right(Right(Left(d)))
      case f: FastTurnaround     => Right(Right(Right(Left(f))))
      case l: LargeProgram       => Right(Right(Right(Right(Left(l)))))
      case p: PoorWeather        => Right(Right(Right(Right(Right(Left(p))))))
      case q: Queue              => Right(Right(Right(Right(Right(Right(Left(q)))))))
      case s: SystemVerification => Right(Right(Right(Right(Right(Right(Right(s)))))))
    }

object ArbProposalType extends ArbProposalType
