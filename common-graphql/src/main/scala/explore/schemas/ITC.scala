// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.annotation.GraphQLSchema
import explore.model.AirMassRange
import explore.model.ElevationRange
import explore.model.HourAngleRange
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.enum
import lucuma.core.model
// gql: import lucuma.ui.reusability._

@GraphQLSchema
trait ITC {
  implicit val piEncoder: Encoder[eu.timepit.refined.types.numeric.PosInt] =
    Encoder.encodeInt.contramap[eu.timepit.refined.types.numeric.PosInt](_.value)

  implicit val pdEncoder: Encoder[eu.timepit.refined.types.numeric.PosBigDecimal] =
    Encoder.encodeBigDecimal.contramap[eu.timepit.refined.types.numeric.PosBigDecimal](_.value)

  implicit val spEncoder: Encoder[model.SpatialProfile] = new Encoder[model.SpatialProfile] {
    final def apply(a: model.SpatialProfile): Json = Json.obj(
      ("sourceType",
       Json.fromString(a match {
         case model.SpatialProfile.PointSource       => "POINT_SOURCE"
         case model.SpatialProfile.UniformSource     => "UNIFORM_SOURCE"
         case model.SpatialProfile.GaussianSource(_) => "GAUSSIAN_SOURCE"
       })
      ),
      ("fwhm",
       a match {
         case model.SpatialProfile.GaussianSource(f) =>
           Json.obj(("microarcseconds", Json.fromString(f.toMicroarcseconds.toString)))
         case _                                      => Json.Null
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
    type PosInt        = eu.timepit.refined.types.numeric.PosInt
    type PosBigDecimal = eu.timepit.refined.types.numeric.PosBigDecimal
  }

  object Enums {
    type MagnitudeBand              = enum.MagnitudeBand
    type MagnitudeSystem            = enum.MagnitudeSystem
    type ImageQuality               = enum.ImageQuality
    type CloudExtinction            = enum.CloudExtinction
    type WaterVapor                 = enum.WaterVapor
    type SkyBackground              = enum.SkyBackground
    type GmosNorthDisperser         = enum.GmosNorthDisperser
    type GmosNorthFilter            = enum.GmosNorthFilter
    type GmosNorthFpu               = enum.GmosNorthFpu
    type GmosCustomSlitWidth        = enum.GmosCustomSlitWidth
    type StellarLibrarySpectrum     = enum.StellarLibrarySpectrum
    type NoneStellarLibrarySpectrum = enum.NonStellarLibrarySpectrum
  }

  object Types {
    type SpatialProfileModelInput  = model.SpatialProfile
    type SpectralDistributionInput = model.SpectralDistribution
    type ConstraintsSetInput       = explore.model.ConstraintSet
  }

}
