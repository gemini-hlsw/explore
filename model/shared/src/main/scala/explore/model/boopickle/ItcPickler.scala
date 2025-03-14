// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import boopickle.Pickler
import cats.implicits.*
import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.itc.ItcAsterismGraphResults
import explore.model.itc.ItcExposureTime
import explore.model.itc.ItcGraphRequestParams
import explore.model.itc.ItcGraphResult
import explore.model.itc.ItcQueryProblem
import explore.model.itc.ItcRequestParams
import explore.model.itc.ItcResult
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.model.itc.OverridenExposureTime
import explore.modes.*
import lucuma.core.data.Zipper
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
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Gid
import lucuma.core.util.Of
import lucuma.core.util.TimeSpan
import lucuma.itc.Error
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.ItcGraph
import lucuma.itc.ItcSeries
import lucuma.itc.ItcVersions
import lucuma.itc.ItcWarning
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.SingleSN
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TotalSN
import lucuma.itc.client.AsterismTargetGraphsResultOutcomes
import lucuma.itc.client.GraphResult
import lucuma.itc.client.SeriesResult
import lucuma.itc.client.SpectroscopyGraphsResult
import lucuma.itc.client.TargetGraphs
import lucuma.itc.client.TargetGraphsResult
import lucuma.itc.client.TargetGraphsResultOutcome
import lucuma.itc.client.TargetInput
import lucuma.itc.client.TargetTimeAndGraphsResult

import scala.collection.immutable.SortedMap

// Boopicklers for itc related types
trait ItcPicklers extends CommonPicklers {

  given Pickler[TotalSN]  = transformPickler((s: SignalToNoise) => TotalSN(s))(_.value)
  given Pickler[SingleSN] = transformPickler((s: SignalToNoise) => SingleSN(s))(_.value)

  given Pickler[GmosCcdMode] = generatePickler

  given Pickler[InstrumentOverrides.GmosSpectroscopy] = generatePickler

  given Pickler[InstrumentConfig.GmosNorthSpectroscopy] = generatePickler

  given Pickler[InstrumentConfig.GmosSouthSpectroscopy] = generatePickler

  given Pickler[InstrumentConfig.Flamingos2Spectroscopy] = generatePickler

  given Pickler[InstrumentConfig.GpiSpectroscopy] = generatePickler

  given Pickler[InstrumentConfig.GnirsSpectroscopy] = generatePickler

  given Pickler[InstrumentConfig.GenericSpectroscopy] = generatePickler

  given Pickler[InstrumentConfig] =
    compositePickler[InstrumentConfig]
      .addConcreteType[InstrumentConfig.GmosNorthSpectroscopy]
      .addConcreteType[InstrumentConfig.GmosSouthSpectroscopy]
      .addConcreteType[InstrumentConfig.Flamingos2Spectroscopy]
      .addConcreteType[InstrumentConfig.GpiSpectroscopy]
      .addConcreteType[InstrumentConfig.GnirsSpectroscopy]
      .addConcreteType[InstrumentConfig.GenericSpectroscopy]

  given Pickler[ModeWavelength] = picklerNewType(ModeWavelength)

  given Pickler[ModeSlitSize] = picklerNewType(ModeSlitSize)

  given Pickler[SlitLength] = picklerNewType(SlitLength)

  given Pickler[SlitWidth] = picklerNewType(SlitWidth)

  given Pickler[ModeAO] = picklerNewType(ModeAO)

  given [A: Gid]: Pickler[A] =
    transformPickler((str: String) => Gid[A].fromString.getOption(str).get)(_.toString)

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

  given Pickler[UnnormalizedSED.UserDefinedAttachment] = generatePickler

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
      .addConcreteType[UnnormalizedSED.UserDefinedAttachment]

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

  given Pickler[TargetInput] = generatePickler

  given Pickler[ItcTarget] = generatePickler

  given Pickler[ItcResult.Pending.type] = generatePickler
  given Pickler[ItcResult.Result]       = generatePickler

  given Pickler[ItcResult] =
    compositePickler[ItcResult]
      .addConcreteType[ItcResult.Pending.type]
      .addConcreteType[ItcResult.Result]

  given Pickler[ItcQueryProblem.UnsupportedMode.type]        = generatePickler
  given Pickler[ItcQueryProblem.MissingWavelength.type]      = generatePickler
  given Pickler[ItcQueryProblem.MissingSignalToNoise.type]   = generatePickler
  given Pickler[ItcQueryProblem.MissingSignalToNoiseAt.type] = generatePickler
  given Pickler[ItcQueryProblem.MissingTargetInfo.type]      = generatePickler
  given Pickler[ItcQueryProblem.MissingBrightness.type]      = generatePickler
  given Pickler[ItcQueryProblem.SourceTooBright]             = generatePickler
  given Pickler[ItcQueryProblem.GenericError]                = generatePickler

  given Pickler[ItcTargetProblem] = generatePickler

  given Pickler[ItcQueryProblem] =
    compositePickler[ItcQueryProblem]
      .addConcreteType[ItcQueryProblem.UnsupportedMode.type]
      .addConcreteType[ItcQueryProblem.MissingWavelength.type]
      .addConcreteType[ItcQueryProblem.MissingSignalToNoise.type]
      .addConcreteType[ItcQueryProblem.MissingSignalToNoiseAt.type]
      .addConcreteType[ItcQueryProblem.MissingTargetInfo.type]
      .addConcreteType[ItcQueryProblem.MissingBrightness.type]
      .addConcreteType[ItcQueryProblem.SourceTooBright]
      .addConcreteType[ItcQueryProblem.GenericError]

  given Pickler[ItcRequestParams] = generatePickler

  given Pickler[ItcAxis] = generatePickler

  given Pickler[SeriesResult] = generatePickler

  given Pickler[GraphResult] = generatePickler

  given Pickler[OverridenExposureTime] = picklerNewType(OverridenExposureTime)

  given Pickler[ItcExposureTime] = generatePickler

  given [A: Pickler]: Pickler[Zipper[A]] =
    transformPickler[Zipper[A], (List[A], A, List[A])]((lefts, focus, rights) =>
      Zipper(lefts, focus, rights)
    )(z => (z.lefts, z.focus, z.rights))

  given Pickler[IntegrationTime] = generatePickler

  given Pickler[SignalToNoiseAt] = generatePickler

  given Pickler[TargetIntegrationTime] = generatePickler

  given Pickler[ItcSeries] = generatePickler

  given Pickler[ItcGraph] = generatePickler

  given Pickler[TargetGraphs] = generatePickler

  given Pickler[TargetTimeAndGraphsResult] = generatePickler

  given Pickler[ItcGraphResult] = generatePickler

  given Pickler[ItcGraphRequestParams] = generatePickler

  given Pickler[ItcWarning] = generatePickler

  given Pickler[ItcCcd] = generatePickler

  given Pickler[TargetGraphsResult] = generatePickler

  given errorSourceTooBright: Pickler[Error.SourceTooBright] = generatePickler
  given errorGeneral: Pickler[Error.General]                 = generatePickler

  given Pickler[Error] = compositePickler[Error]
    .addConcreteType[Error.SourceTooBright]
    .addConcreteType[Error.General]

  given Pickler[TargetGraphsResultOutcome] = picklerNewType(TargetGraphsResultOutcome)

  given Pickler[AsterismTargetGraphsResultOutcomes] = picklerNewType(
    AsterismTargetGraphsResultOutcomes
  )

  given Pickler[SpectroscopyGraphsResult] = generatePickler

  given Pickler[ItcAsterismGraphResults] = generatePickler

  given Pickler[ItcVersions] = generatePickler
}

object ItcPicklers extends ItcPicklers
