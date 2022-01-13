// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.annotation.GraphQLSchema
import eu.timepit.refined.types.numeric
import explore.model.AirMassRange
import explore.model.ElevationRange
import explore.model.HourAngleRange
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.`enum`
import lucuma.core.model
// gql: import lucuma.ui.reusability._

@GraphQLSchema
trait ITC {
  implicit val piEncoder: Encoder[numeric.PosInt] =
    Encoder.encodeInt.contramap[numeric.PosInt](_.value)

  implicit val pdEncoder: Encoder[numeric.PosBigDecimal] =
    Encoder.encodeBigDecimal.contramap[numeric.PosBigDecimal](_.value)

  implicit val spEncoder: Encoder[model.SourceProfile] = new Encoder[model.SourceProfile] {
    final def apply(a: model.SourceProfile): Json = Json.obj(
      ("sourceType",
       Json.fromString(a match {
         case model.SourceProfile.Point(_)       => "POINT_SOURCE"
         case model.SourceProfile.Uniform(_)     => "UNIFORM_SOURCE"
         case model.SourceProfile.Gaussian(_, _) => "GAUSSIAN_SOURCE"
       })
      ),
      ("fwhm",
       a match {
         case model.SourceProfile.Gaussian(f, _) =>
           Json.obj(("microarcseconds", Json.fromString(f.toMicroarcseconds.toString)))
         case _                                  => Json.Null
       }
      )
    )
  }

  implicit val bbEncoder: Encoder[model.SpectralDistribution.BlackBody] =
    new Encoder[model.SpectralDistribution.BlackBody] {
      final def apply(a: model.SpectralDistribution.BlackBody): Json = Json.obj(
        ("blackBody", Json.obj(("temperature", Json.fromBigDecimal(a.temperature.value.value))))
      )
    }

  implicit val plEncoder: Encoder[model.SpectralDistribution.PowerLaw] =
    new Encoder[model.SpectralDistribution.PowerLaw] {
      final def apply(a: model.SpectralDistribution.PowerLaw): Json = Json.obj(
        ("powerLaw", Json.obj(("index", Json.fromBigDecimal(a.index))))
      )
    }

  implicit val lEncoder: Encoder[model.SpectralDistribution.Library] =
    new Encoder[model.SpectralDistribution.Library] {
      final def apply(a: model.SpectralDistribution.Library): Json =
        a.librarySpectrum match {
          case Left(s: enum.StellarLibrarySpectrum)     =>
            Json.obj(("stellar", Json.fromString(s.ocs2Tag)))
          case Right(n: enum.NonStellarLibrarySpectrum) =>
            Json.obj(("nonStellar", Json.fromString(n.ocs2Tag)))
        }
    }

  implicit val sdEncoder: Encoder[model.SpectralDistribution] = Encoder.instance {
    case bb: model.SpectralDistribution.BlackBody => bb.asJson
    case pl: model.SpectralDistribution.PowerLaw  => pl.asJson
    case l: model.SpectralDistribution.Library    => l.asJson
  }

  implicit val erEncoder: Encoder[ElevationRange] = Encoder.instance {
    case AirMassRange(mi, ma)   =>
      Json.obj(
        ("airmassRange",
         Json.obj(("min", Json.fromBigDecimal(mi.value)), ("max", Json.fromBigDecimal(ma.value)))
        )
      )
    case HourAngleRange(mi, ma) =>
      Json.obj(
        ("hourAngleRange",
         Json.obj(("minHours", Json.fromBigDecimal(mi.value)),
                  ("maxHours", Json.fromBigDecimal(ma.value))
         )
        )
      )
  }

  implicit val csEncoder: Encoder[explore.model.ConstraintSet] = deriveEncoder

  object Scalars {
    // Basic types
    type BigDecimal    = scala.BigDecimal
    type Long          = scala.Long
    // Time
    type Instant       = java.time.Instant
    // Refined
    type PosInt        = numeric.PosInt
    type PosBigDecimal = numeric.PosBigDecimal
  }

  object Enums {
    type Band                       = enum.Band
    type ImageQuality               = enum.ImageQuality
    type CloudExtinction            = enum.CloudExtinction
    type WaterVapor                 = enum.WaterVapor
    type SkyBackground              = enum.SkyBackground
    type GmosNorthDisperser         = enum.GmosNorthDisperser
    type GmosNorthFilter            = enum.GmosNorthFilter
    type GmosNorthFpu               = enum.GmosNorthFpu
    type GmosSouthDisperser         = enum.GmosSouthDisperser
    type GmosSouthFilter            = enum.GmosSouthFilter
    type GmosSouthFpu               = enum.GmosSouthFpu
    type GmosCustomSlitWidth        = enum.GmosCustomSlitWidth
    type StellarLibrarySpectrum     = enum.StellarLibrarySpectrum
    type NoneStellarLibrarySpectrum = enum.NonStellarLibrarySpectrum
  }

  object Types {
    type SpectralDistributionInput = model.SpectralDistribution
    type ConstraintsSetInput       = explore.model.ConstraintSet
  }

}
