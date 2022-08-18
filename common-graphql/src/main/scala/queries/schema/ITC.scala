// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import clue.annotation.GraphQLSchema
import eu.timepit.refined.types.numeric
import eu.timepit.refined.types.string
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto._
import lucuma.core.enums
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.model
import lucuma.core.model._
import lucuma.core.model.sequence._

@GraphQLSchema
trait ITC {
  given Encoder[numeric.PosInt] =
    Encoder.encodeInt.contramap[numeric.PosInt](_.value)

  given Encoder[numeric.PosBigDecimal] =
    Encoder.encodeBigDecimal.contramap[numeric.PosBigDecimal](_.value)

  given Encoder[numeric.PosLong] =
    Encoder.encodeLong.contramap[numeric.PosLong](_.value)

  given Encoder[numeric.NonNegBigDecimal] =
    Encoder.encodeBigDecimal.contramap[numeric.NonNegBigDecimal](_.value)

  given Encoder[model.SourceProfile] = (a: model.SourceProfile) =>
    Json.obj(
      ("sourceType",
       Json.fromString(a match
         case model.SourceProfile.Point(_)       => "POINT_SOURCE"
         case model.SourceProfile.Uniform(_)     => "UNIFORM_SOURCE"
         case model.SourceProfile.Gaussian(_, _) => "GAUSSIAN_SOURCE"
       )
      ),
      ("fwhm",
       a match
         case model.SourceProfile.Gaussian(f, _) =>
           Json.obj(("microarcseconds", Json.fromString(f.toMicroarcseconds.toString)))
         case _                                  => Json.Null
      )
    )

  given Encoder[ElevationRange] = Encoder.instance {
    case ElevationRange.AirMass(mi, ma)   =>
      Json.obj(
        ("airMass",
         Json.obj(("min", Json.fromBigDecimal(mi.value)), ("max", Json.fromBigDecimal(ma.value)))
        )
      )
    case ElevationRange.HourAngle(mi, ma) =>
      Json.obj(
        ("hourAngle",
         Json.obj(("minHours", Json.fromBigDecimal(mi.value)),
                  ("maxHours", Json.fromBigDecimal(ma.value))
         )
        )
      )
  }

  given Encoder[ConstraintSet] = deriveEncoder

  object Scalars {
    // Ids
    type AtomId           = Atom.Id
    type ExecutionEventId = ExecutionEvent.Id
    type ObservationId    = Observation.Id
    type ProgramId        = Program.Id
    type StepId           = Step.Id
    type TargetId         = Target.Id
    type VisitId          = Visit.Id
    // Basic types
    type BigDecimal       = scala.BigDecimal
    type Long             = scala.Long
    // Formatted strings
    type DatasetFilename  = String
    type DmsString        = String
    type EpochString      = String
    type HmsString        = String
    // Time
    type Instant          = java.time.Instant
    // Refined
    type NonEmptyString   = string.NonEmptyString
    type PosInt           = numeric.PosInt
    type PosLong          = numeric.PosLong
    type PosBigDecimal    = numeric.PosBigDecimal
    type NonNegBigDecimal = numeric.NonNegBigDecimal
  }

  object Enums {
    type Band                                = enums.Band
    type BrightnessIntegratedUnits           = Units Of Brightness[Integrated]
    type BrightnessSurfaceUnits              = Units Of Brightness[Surface]
    type CoolStarTemperature                 = enums.CoolStarTemperature
    type FluxDensityContinuumIntegratedUnits = Units Of FluxDensityContinuum[Integrated]
    type FluxDensityContinuumSurfaceUnits    = Units Of FluxDensityContinuum[Surface]
    type HiiRegionSpectrum                   = enums.HIIRegionSpectrum
    type GalaxySpectrum                      = enums.GalaxySpectrum
    type LineFluxIntegratedUnits             = Units Of LineFlux[Integrated]
    type PlanetSpectrum                      = enums.PlanetSpectrum
    type PlanetaryNebulaSpectrum             = enums.PlanetaryNebulaSpectrum
    type QuasarSpectrum                      = enums.QuasarSpectrum
    type LineFluxSurfaceUnits                = Units Of LineFlux[Surface]
    type ImageQuality                        = enums.ImageQuality
    type CloudExtinction                     = enums.CloudExtinction
    type WaterVapor                          = enums.WaterVapor
    type SkyBackground                       = enums.SkyBackground
    type GmosNorthGrating                    = enums.GmosNorthGrating
    type GmosNorthFilter                     = enums.GmosNorthFilter
    type GmosNorthBuiltinFpu                 = enums.GmosNorthFpu
    type GmosSouthGrating                    = enums.GmosSouthGrating
    type GmosSouthFilter                     = enums.GmosSouthFilter
    type GmosSouthBuiltinFpu                 = enums.GmosSouthFpu
    type GmosCustomSlitWidth                 = enums.GmosCustomSlitWidth
    type StellarLibrarySpectrum              = enums.StellarLibrarySpectrum
    type ItcChartDataType                    = explore.model.enums.ItcSeriesDataType
  }

  object Types {
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
