// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import cats.implicits.*
import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcExposureTime
import explore.model.itc.ItcGraphRequestParams
import explore.model.itc.ItcQueryProblems
import explore.model.itc.ItcRequestParams
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.modes.*
import explore.modes.InstrumentRow
import explore.modes.ModeAO
import explore.modes.ModeSlitSize
import explore.modes.ModeWavelength
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.BrightnessUnits.Brightness
import lucuma.core.math.BrightnessUnits.FluxDensityContinuum
import lucuma.core.math.BrightnessUnits.FluxDensityContinuumMeasure
import lucuma.core.math.BrightnessUnits.LineFlux
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Of
import lucuma.core.util.TimeSpan
import lucuma.itc.FinalSN
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.ItcWarning
import lucuma.itc.SingleSN
import lucuma.itc.client.ItcVersions
import lucuma.itc.client.OptimizedChartResult
import lucuma.itc.client.OptimizedSeriesResult
import lucuma.itc.client.OptimizedSpectroscopyGraphResult

import scala.collection.immutable.SortedMap

// Boopicklers for itc related types
trait ItcPicklers extends CommonPicklers {

  given Pickler[FinalSN]  = transformPickler((s: SignalToNoise) => FinalSN(s))(_.value)
  given Pickler[SingleSN] = transformPickler((s: SignalToNoise) => SingleSN(s))(_.value)

  given Pickler[GmosCcdMode] = generatePickler

  given Pickler[GmosSpectroscopyOverrides] = generatePickler

  given Pickler[GmosNorthSpectroscopyRow] = generatePickler

  given Pickler[GmosSouthSpectroscopyRow] = generatePickler

  given Pickler[Flamingos2SpectroscopyRow] = generatePickler

  given Pickler[GpiSpectroscopyRow] = generatePickler

  given Pickler[GnirsSpectroscopyRow] = generatePickler

  given Pickler[GenericSpectroscopyRow] = generatePickler

  given Pickler[InstrumentRow] =
    compositePickler[InstrumentRow]
      .addConcreteType[GmosNorthSpectroscopyRow]
      .addConcreteType[GmosSouthSpectroscopyRow]
      .addConcreteType[Flamingos2SpectroscopyRow]
      .addConcreteType[GpiSpectroscopyRow]
      .addConcreteType[GnirsSpectroscopyRow]
      .addConcreteType[GenericSpectroscopyRow]

  given Pickler[ModeWavelength] = picklerNewType(ModeWavelength)

  given Pickler[ModeSlitSize] = picklerNewType(ModeSlitSize)

  given Pickler[SlitLength] = picklerNewType(SlitLength)

  given Pickler[SlitWidth] = picklerNewType(SlitWidth)

  given Pickler[ModeWavelengthDelta] = picklerNewType(ModeWavelengthDelta)

  given Pickler[SpectroscopyModeRow] = generatePickler

  given Pickler[SpectroscopyModesMatrix] = generatePickler

  given Pickler[UnnormalizedSED.StellarLibrary] = generatePickler

  given Pickler[UnnormalizedSED.CoolStarModel] = generatePickler

  given Pickler[UnnormalizedSED.Galaxy] = generatePickler

  given Pickler[UnnormalizedSED.Planet] = generatePickler

  given Pickler[UnnormalizedSED.Quasar] = generatePickler

  given Pickler[UnnormalizedSED.HIIRegion] = generatePickler

  given Pickler[UnnormalizedSED.PlanetaryNebula] = generatePickler

  given Pickler[UnnormalizedSED.PowerLaw] = generatePickler

  given Pickler[UnnormalizedSED.BlackBody] = generatePickler

  given Pickler[UnnormalizedSED.UserDefined] = generatePickler

  given Pickler[UnnormalizedSED] =
    compositePickler[UnnormalizedSED]
      .addConcreteType[UnnormalizedSED.StellarLibrary]
      .addConcreteType[UnnormalizedSED.CoolStarModel]
      .addConcreteType[UnnormalizedSED.Galaxy]
      .addConcreteType[UnnormalizedSED.Planet]
      .addConcreteType[UnnormalizedSED.Quasar]
      .addConcreteType[UnnormalizedSED.HIIRegion]
      .addConcreteType[UnnormalizedSED.PlanetaryNebula]
      .addConcreteType[UnnormalizedSED.PowerLaw]
      .addConcreteType[UnnormalizedSED.BlackBody]
      .addConcreteType[UnnormalizedSED.UserDefined]

  given taggedMeasurePickler[N: Pickler, T](using Pickler[Units Of T]): Pickler[Measure[N] Of T] =
    transformPickler { (x: (Units Of T, N, Option[N])) =>
      val base = x._1.withValueTagged(x._2)
      x._3.map(base.withError).getOrElse(base)
    }(x => (Measure.unitsTagged.get(x), x.value, x.error))

  given bandNormalizedPickler[A](using
    Pickler[Units Of Brightness[A]]
  ): Pickler[SpectralDefinition.BandNormalized[A]] =
    generatePickler

  given emissionLinesPickler[A](using Pickler[Units Of LineFlux[A]]): Pickler[EmissionLine[A]] =
    generatePickler

  given Pickler[LineFluxValue] = picklerNewType(LineFluxValue)

  given Pickler[LineWidthValue] = picklerNewType(LineWidthValue)

  given Pickler[FluxDensityContinuumValue] = picklerNewType(FluxDensityContinuumValue)

  given spectralEmissionLinesPickler[A](using
    Pickler[Units Of LineFlux[A]],
    Pickler[Units Of FluxDensityContinuum[A]]
  ): Pickler[SpectralDefinition.EmissionLines[A]] =
    transformPickler((x: (List[(Wavelength, EmissionLine[A])], FluxDensityContinuumMeasure[A])) =>
      SpectralDefinition.EmissionLines(SortedMap.from(x._1), x._2)
    )(x => (x.lines.toList, x.fluxDensityContinuum))

  given spectralDefinitionPickler[A](using
    Pickler[Units Of Brightness[A]],
    Pickler[Units Of LineFlux[A]],
    Pickler[Units Of FluxDensityContinuum[A]]
  ): Pickler[SpectralDefinition[A]] =
    compositePickler[SpectralDefinition[A]]
      .addConcreteType[SpectralDefinition.BandNormalized[A]]
      .addConcreteType[SpectralDefinition.EmissionLines[A]]

  given Pickler[SourceProfile.Point] = generatePickler

  given Pickler[SourceProfile.Uniform] = generatePickler

  given Pickler[SourceProfile.Gaussian] = generatePickler

  given Pickler[SourceProfile] =
    compositePickler[SourceProfile]
      .addConcreteType[SourceProfile.Point]
      .addConcreteType[SourceProfile.Uniform]
      .addConcreteType[SourceProfile.Gaussian]

  given Pickler[ItcTarget] = generatePickler

  given Pickler[ItcResult.Pending.type] = generatePickler
  given Pickler[ItcResult.Result]       = generatePickler

  given Pickler[ItcResult] =
    compositePickler[ItcResult]
      .addConcreteType[ItcResult.Pending.type]
      .addConcreteType[ItcResult.Result]

  given Pickler[ItcQueryProblems.UnsupportedMode.type]        = generatePickler
  given Pickler[ItcQueryProblems.MissingWavelength.type]      = generatePickler
  given Pickler[ItcQueryProblems.MissingSignalToNoise.type]   = generatePickler
  given Pickler[ItcQueryProblems.MissingSignalToNoiseAt.type] = generatePickler
  given Pickler[ItcQueryProblems.MissingTargetInfo.type]      = generatePickler
  given Pickler[ItcQueryProblems.MissingBrightness.type]      = generatePickler
  given Pickler[ItcQueryProblems.SourceTooBright]             = generatePickler
  given Pickler[ItcQueryProblems.GenericError]                = generatePickler

  given Pickler[ItcQueryProblems] =
    compositePickler[ItcQueryProblems]
      .addConcreteType[ItcQueryProblems.UnsupportedMode.type]
      .addConcreteType[ItcQueryProblems.MissingWavelength.type]
      .addConcreteType[ItcQueryProblems.MissingSignalToNoise.type]
      .addConcreteType[ItcQueryProblems.MissingSignalToNoiseAt.type]
      .addConcreteType[ItcQueryProblems.MissingTargetInfo.type]
      .addConcreteType[ItcQueryProblems.MissingBrightness.type]
      .addConcreteType[ItcQueryProblems.SourceTooBright]
      .addConcreteType[ItcQueryProblems.GenericError]

  given Pickler[ItcRequestParams] = generatePickler

  given Pickler[ItcAxis] = generatePickler

  given Pickler[OptimizedSeriesResult] = generatePickler

  given Pickler[OptimizedChartResult] = generatePickler

  given Pickler[OverridenExposureTime] = picklerNewType(OverridenExposureTime)

  given Pickler[ItcExposureTime] = generatePickler

  given Pickler[ItcChartResult] = generatePickler

  given Pickler[ItcGraphRequestParams] = generatePickler

  given Pickler[ItcWarning] = generatePickler

  given Pickler[ItcCcd] = generatePickler

  given Pickler[OptimizedSpectroscopyGraphResult] = generatePickler

  given Pickler[ItcVersions] = generatePickler
}

object ItcPicklers extends ItcPicklers
