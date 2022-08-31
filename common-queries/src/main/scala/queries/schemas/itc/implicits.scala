// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.itc

import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import explore.common.ObsQueries
import explore.model.itc.ItcTarget
import explore.model.TargetWithId
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import explore.optics.all._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math._
import lucuma.core.math.dimensional.Measure
import lucuma.core.model._
import lucuma.core.optics.syntax.lens._
import queries.common.ITCQueriesGQL
import queries.schemas.ITC
import queries.schemas.ITC.Types._
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import cats.data.NonEmptyList
import explore.model.Asterism
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.numeric.NonNegBigDecimal

// There is a lot of duplication here with the odb.implicits package
object implicits {
  type SpectroscopyModeInput = ITC.Types.SpectroscopyModeInput
  val SpectroscopyModeInput = ITC.Types.SpectroscopyModeInput
  type GmosNorthFpuInput = ITC.Types.GmosNorthFpuInput
  val GmosNorthFpuInput = ITC.Types.GmosNorthFpuInput
  type GmosSouthFpuInput = ITC.Types.GmosSouthFpuInput
  val GmosSouthFpuInput = ITC.Types.GmosSouthFpuInput
  type ItcResults = ITCQueriesGQL.SpectroscopyITCQuery.Data
  type ItcError   = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcError
  val ItcError = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcError
  type ItcSuccess = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcSuccess
  val ItcSuccess = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcSuccess

  type SpectroscopyGraphModeInput = ITC.Types.SpectroscopyGraphModeInput
  val SpectroscopyGraphModeInput = ITC.Types.SpectroscopyGraphModeInput
  type SignificantFiguresInput = ITC.Types.SignificantFiguresInput
  val SignificantFiguresInput = ITC.Types.SignificantFiguresInput
  type ItcGraphResults = ITCQueriesGQL.SpectroscopyGraphITCQuery.Data

  extension (a: Angle)
    def toInput: AngleInput =
      AngleInput(microarcseconds = a.toMicroarcseconds.assign)

  extension (w: Wavelength)
    def toInput: WavelengthInput =
      (WavelengthInput.nanometers := Wavelength.decimalNanometers
        .reverseGet(w)
        .assign)
        .runS(WavelengthInput())
        .value

  extension (nnd: NonNegDuration)
    def toInput: NonNegDurationInput =
      NonNegDurationInput(milliseconds = NonNegBigDecimal.unsafeFrom(nnd.value.toMillis).assign)

  // These are copied from the odb side
  extension (u: UnnormalizedSED)
    def toInput: UnnormalizedSedInput =
      u match
        case UnnormalizedSED.StellarLibrary(librarySpectrum)          =>
          UnnormalizedSedInput(stellarLibrary = librarySpectrum.assign)
        case UnnormalizedSED.CoolStarModel(temperature)               =>
          UnnormalizedSedInput(coolStar = temperature.assign)
        case UnnormalizedSED.Galaxy(galaxySpectrum)                   =>
          UnnormalizedSedInput(galaxy = galaxySpectrum.assign)
        case UnnormalizedSED.Planet(planetSpectrum)                   =>
          UnnormalizedSedInput(planet = planetSpectrum.assign)
        case UnnormalizedSED.Quasar(quasarSpectrum)                   =>
          UnnormalizedSedInput(quasar = quasarSpectrum.assign)
        case UnnormalizedSED.HIIRegion(hiiRegionSpectrum)             =>
          UnnormalizedSedInput(hiiRegion = hiiRegionSpectrum.assign)
        case UnnormalizedSED.PlanetaryNebula(planetaryNebulaSpectrum) =>
          UnnormalizedSedInput(planetaryNebula = planetaryNebulaSpectrum.assign)
        case UnnormalizedSED.PowerLaw(index)                          =>
          UnnormalizedSedInput(powerLaw = index.assign)
        case UnnormalizedSED.BlackBody(temperature)                   =>
          UnnormalizedSedInput(blackBodyTempK = temperature.value.assign)
        case UnnormalizedSED.UserDefined(fluxDensities)               =>
          UnnormalizedSedInput(fluxDensities = fluxDensities.toSortedMap.toList.map {
            case (wavelength, value) => FluxDensity(wavelength.toInput, value)
          }.assign)

  extension (b: SpectralDefinition.BandNormalized[Integrated])
    def toCreateInput: BandNormalizedIntegratedInput =
      BandNormalizedIntegratedInput(
        sed = b.sed.toInput.assign,
        brightnesses = b.brightnesses.toList.map { (band, measure) =>
          BandBrightnessIntegratedInput(
            band = band,
            value = measure.value.assign,
            units = Measure.unitsTagged.get(measure).assign,
            error = measure.error.orIgnore
          )
        }.assign
      )

  extension (b: SpectralDefinition.BandNormalized[Surface])
    def toCreateInput: BandNormalizedSurfaceInput =
      BandNormalizedSurfaceInput(
        sed = b.sed.toInput.assign,
        brightnesses = b.brightnesses.toList.map { (band, measure) =>
          BandBrightnessSurfaceInput(
            band = band,
            value = measure.value.assign,
            units = Measure.unitsTagged.get(measure).assign,
            error = measure.error.orIgnore
          )
        }.assign
      )

  extension (e: SpectralDefinition.EmissionLines[Integrated])
    def toCreateInput: EmissionLinesIntegratedInput =
      EmissionLinesIntegratedInput(
        lines = e.lines.toList.map { (wavelength, line) =>
          EmissionLineIntegratedInput(
            wavelength = wavelength.toInput,
            lineWidth = line.lineWidth.value.assign,
            lineFlux = LineFluxIntegratedInput(
              line.lineFlux.value,
              Measure.unitsTagged.get(line.lineFlux)
            ).assign
          )
        }.assign,
        fluxDensityContinuum = FluxDensityContinuumIntegratedInput(
          value = e.fluxDensityContinuum.value,
          units = Measure.unitsTagged.get(e.fluxDensityContinuum)
        ).assign
      )

  extension (e: GmosNorthFpu)
    def toInput: GmosNorthFpuInput =
      GmosNorthFpuInput(builtin = e.assign)

  extension (e: GmosSouthFpu)
    def toInput: GmosSouthFpuInput =
      GmosSouthFpuInput(builtin = e.assign)

  extension (e: SpectralDefinition.EmissionLines[Surface])
    def toCreateInput: EmissionLinesSurfaceInput =
      EmissionLinesSurfaceInput(
        lines = e.lines.toList.map { (wavelength, line) =>
          EmissionLineSurfaceInput(
            wavelength = wavelength.toInput,
            lineWidth = line.lineWidth.value.assign,
            lineFlux = LineFluxSurfaceInput(
              line.lineFlux.value,
              Measure.unitsTagged.get(line.lineFlux)
            ).assign
          )
        }.assign,
        fluxDensityContinuum = FluxDensityContinuumSurfaceInput(
          value = e.fluxDensityContinuum.value,
          units = Measure.unitsTagged.get(e.fluxDensityContinuum)
        ).assign
      )

  extension (s: SpectralDefinition[Integrated])
    def toCreateInput: SpectralDefinitionIntegratedInput =
      s match
        case b @ SpectralDefinition.BandNormalized(_, _) =>
          SpectralDefinitionIntegratedInput(bandNormalized = b.toCreateInput.assign)
        case e @ SpectralDefinition.EmissionLines(_, _)  =>
          SpectralDefinitionIntegratedInput(emissionLines = e.toCreateInput.assign)

  extension (s: SpectralDefinition[Surface])
    def toCreateInput: SpectralDefinitionSurfaceInput =
      s match
        case b @ SpectralDefinition.BandNormalized(_, _) =>
          SpectralDefinitionSurfaceInput(bandNormalized = b.toCreateInput.assign)
        case e @ SpectralDefinition.EmissionLines(_, _)  =>
          SpectralDefinitionSurfaceInput(emissionLines = e.toCreateInput.assign)

  extension (s: SourceProfile)
    def toInput: SourceProfileInput =
      s match
        case SourceProfile.Point(definition)          =>
          SourceProfileInput(point = definition.toCreateInput.assign)
        case SourceProfile.Uniform(definition)        =>
          SourceProfileInput(uniform = definition.toCreateInput.assign)
        case SourceProfile.Gaussian(fwhm, definition) =>
          SourceProfileInput(gaussian =
            GaussianInput(fwhm.toInput.assign, definition.toCreateInput.assign).assign
          )

  extension (r: GmosNorthSpectroscopyRow)
    def toGmosNITCInput: Input[GmosNITCInput] =
      GmosNITCInput(r.grating, r.fpu.toInput, filter = r.filter.orIgnore).assign

  extension (r: GmosSouthSpectroscopyRow)
    def toGmosSITCInput: Input[GmosSITCInput] =
      GmosSITCInput(r.grating, r.fpu.toInput, filter = r.filter.orIgnore).assign

  extension (r: RadialVelocity)
    def toITCInput: RadialVelocityInput =
      RadialVelocityInput(metersPerSecond = r.rv.value.assign)

  extension (s: ObsQueries.ScienceData)
    // From the list of targets selects the ones relevant for ITC
    def itcTargets: List[ItcTarget] = s.targets.asterism
      .map { case TargetWithId(_, target) =>
        targetRV
          .getOption(target)
          .map(r => ItcTarget(target.name, r, Target.sourceProfile.get(target)))
      }
      .flatten
      .hashDistinct

    def baseTarget: Option[Target] =
      NonEmptyList.fromList(s.targets.asterism).map(a => Asterism(a).baseTarget.target)

    def baseSiderealTracking: Option[SiderealTracking] =
      baseTarget match
        case Some(s: Target.Sidereal) => s.tracking.some
        case _                        => none

  extension (m: InstrumentRow)
    def toITCInput: Option[InstrumentModesInput] = m match
      case r: GmosNorthSpectroscopyRow =>
        InstrumentModesInput(gmosN = r.toGmosNITCInput).some
      case r: GmosSouthSpectroscopyRow =>
        InstrumentModesInput(gmosS = r.toGmosSITCInput).some
      case _                           => none
}
