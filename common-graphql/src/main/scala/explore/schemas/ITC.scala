// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.annotation.GraphQLSchema
import eu.timepit.refined.types.numeric
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.`enum`
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.model
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
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
    case ElevationRange.AirMass(mi, ma)   =>
      Json.obj(
        ("airmassRange",
         Json.obj(("min", Json.fromBigDecimal(mi.value)), ("max", Json.fromBigDecimal(ma.value)))
        )
      )
    case ElevationRange.HourAngle(mi, ma) =>
      Json.obj(
        ("hourAngleRange",
         Json.obj(("minHours", Json.fromBigDecimal(mi.value)),
                  ("maxHours", Json.fromBigDecimal(ma.value))
         )
        )
      )
  }

  implicit val csEncoder: Encoder[ConstraintSet] = deriveEncoder

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
    type Band                                = enum.Band
    type BrightnessIntegratedUnits           = Units Of Brightness[Integrated]
    type BrightnessSurfaceUnits              = Units Of Brightness[Surface]
    type CoolStarTemperature                 = enum.CoolStarTemperature
    type FluxDensityContinuumIntegratedUnits = Units Of FluxDensityContinuum[Integrated]
    type FluxDensityContinuumSurfaceUnits    = Units Of FluxDensityContinuum[Surface]
    type HiiRegionSpectrum                   = enum.HIIRegionSpectrum
    type GalaxySpectrum                      = enum.GalaxySpectrum
    type LineFluxIntegratedUnits             = Units Of LineFlux[Integrated]
    type PlanetSpectrum                      = enum.PlanetSpectrum
    type PlanetaryNebulaSpectrum             = enum.PlanetaryNebulaSpectrum
    type QuasarSpectrum                      = enum.QuasarSpectrum
    type LineFluxSurfaceUnits                = Units Of LineFlux[Surface]
    type ImageQuality                        = enum.ImageQuality
    type CloudExtinction                     = enum.CloudExtinction
    type WaterVapor                          = enum.WaterVapor
    type SkyBackground                       = enum.SkyBackground
    type GmosNorthDisperser                  = enum.GmosNorthDisperser
    type GmosNorthFilter                     = enum.GmosNorthFilter
    type GmosNorthFpu                        = enum.GmosNorthFpu
    type GmosSouthDisperser                  = enum.GmosSouthDisperser
    type GmosSouthFilter                     = enum.GmosSouthFilter
    type GmosSouthFpu                        = enum.GmosSouthFpu
    type GmosCustomSlitWidth                 = enum.GmosCustomSlitWidth
    type StellarLibrarySpectrum              = enum.StellarLibrarySpectrum
    type NoneStellarLibrarySpectrum          = enum.NonStellarLibrarySpectrum
  }

  object Types {
    type SpectralDistributionInput      = model.SpectralDistribution
    type ConstraintSetInput             = ConstraintSet
    type BandNormalizedIntegrated       = model.SpectralDefinition.BandNormalized[Integrated]
    type BandNormalizedSurface          = model.SpectralDefinition.BandNormalized[Surface]
    type BrightnessIntegrated           = Measure[BigDecimal] Of Brightness[Integrated]
    type BrightnessSurface              = Measure[BigDecimal] Of Brightness[Surface]
    type BlackBody                      = model.UnnormalizedSED.BlackBody
    type CoolStarModel                  = model.UnnormalizedSED.CoolStarModel
    type EmissionLineIntegrated         = model.EmissionLine[Integrated]
    type EmissionLineSurface            = model.EmissionLine[Surface]
    type EmissionLinesIntegrated        = model.SpectralDefinition.EmissionLines[Integrated]
    type EmissionLinesSurface           = model.SpectralDefinition.EmissionLines[Surface]
    type FluxDensityContinuumIntegrated = Measure[BigDecimal] Of FluxDensityContinuum[Integrated]
    type FluxDensityContinuumSurface    = Measure[BigDecimal] Of FluxDensityContinuum[Surface]
    type Galaxy                         = model.UnnormalizedSED.Galaxy
    type GaussianSource                 = model.SourceProfile.Gaussian
    type HiiRegion                      = model.UnnormalizedSED.HIIRegion
    type LineFluxIntegrated             = Measure[BigDecimal] Of LineFlux[Integrated]
    type LineFluxSurface                = Measure[BigDecimal] Of LineFlux[Surface]
    type Planet                         = model.UnnormalizedSED.Planet
    type PlanetaryNebula                = model.UnnormalizedSED.PlanetaryNebula
    type PointSource                    = model.SourceProfile.Point
    type Quasar                         = model.UnnormalizedSED.Quasar
    type StellarLibrary                 = model.UnnormalizedSED.StellarLibrary
    type UniformSource                  = model.SourceProfile.Uniform
    type UserDefined                    = model.UnnormalizedSED.UserDefined
  }

}
