// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import explore.common.ObsQueries
import explore.model.ITCTarget
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

// There is a lot of duplication here with the odb.implicits package
package itc {

  import lucuma.core.enums.GmosNorthFpu
  import lucuma.core.enums.GmosSouthFpu
  import cats.data.NonEmptyList
  import explore.model.Asterism
  object implicits {
    implicit class AngleOps(val a: Angle) extends AnyVal {
      def toInput: AngleInput =
        AngleInput(microarcseconds = a.toMicroarcseconds.assign)
    }

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

    implicit class WavelengthOps(val w: Wavelength) extends AnyVal {
      def toInput: WavelengthInput =
        (WavelengthInput.nanometers := Wavelength.decimalNanometers
          .reverseGet(w)
          .assign)
          .runS(WavelengthInput())
          .value
    }

    // These are copied from the odb side
    implicit class UnnormalizedSedOps(val u: UnnormalizedSED) extends AnyVal {
      def toInput: UnnormalizedSedInput =
        u match {
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
        }
    }

    implicit class IntegratedBandNormalizedOps(val b: SpectralDefinition.BandNormalized[Integrated])
        extends AnyVal {
      def toCreateInput: BandNormalizedIntegratedInput =
        BandNormalizedIntegratedInput(
          sed = b.sed.toInput.assign,
          brightnesses = b.brightnesses.toList.map { case (band, measure) =>
            BandBrightnessIntegratedInput(
              band = band,
              value = measure.value.assign,
              units = Measure.unitsTagged.get(measure).assign,
              error = measure.error.orIgnore
            )
          }.assign
        )
    }

    implicit class SurfaceBandNormalizedOps(val b: SpectralDefinition.BandNormalized[Surface])
        extends AnyVal {
      def toCreateInput: BandNormalizedSurfaceInput =
        BandNormalizedSurfaceInput(
          sed = b.sed.toInput.assign,
          brightnesses = b.brightnesses.toList.map { case (band, measure) =>
            BandBrightnessSurfaceInput(
              band = band,
              value = measure.value.assign,
              units = Measure.unitsTagged.get(measure).assign,
              error = measure.error.orIgnore
            )
          }.assign
        )
    }

    implicit class IntegratedEmissionLinesOps(val e: SpectralDefinition.EmissionLines[Integrated])
        extends AnyVal {
      def toCreateInput: EmissionLinesIntegratedInput =
        EmissionLinesIntegratedInput(
          lines = e.lines.toList.map { case (wavelength, line) =>
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
    }

    implicit class GmosNorthFpuOps(val e: GmosNorthFpu) extends AnyVal {
      def toInput: GmosNorthFpuInput =
        GmosNorthFpuInput(builtin = e.assign)
    }

    implicit class GmosSouthFpuOps(val e: GmosSouthFpu) extends AnyVal {
      def toInput: GmosSouthFpuInput =
        GmosSouthFpuInput(builtin = e.assign)
    }

    implicit class SurfaceEmissionLinesOps(e: SpectralDefinition.EmissionLines[Surface]) {
      def toCreateInput: EmissionLinesSurfaceInput =
        EmissionLinesSurfaceInput(
          lines = e.lines.toList.map { case (wavelength, line) =>
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
    }
    implicit class IntegratedSpectralDefinitionOps(s: SpectralDefinition[Integrated])    {
      def toCreateInput: SpectralDefinitionIntegratedInput =
        s match {
          case b @ SpectralDefinition.BandNormalized(_, _) =>
            SpectralDefinitionIntegratedInput(bandNormalized = b.toCreateInput.assign)
          case e @ SpectralDefinition.EmissionLines(_, _)  =>
            SpectralDefinitionIntegratedInput(emissionLines = e.toCreateInput.assign)
        }
    }

    implicit class SurfaceSpectralDefinitionOps(s: SpectralDefinition[Surface]) {
      def toCreateInput: SpectralDefinitionSurfaceInput =
        s match {
          case b @ SpectralDefinition.BandNormalized(_, _) =>
            SpectralDefinitionSurfaceInput(bandNormalized = b.toCreateInput.assign)
          case e @ SpectralDefinition.EmissionLines(_, _)  =>
            SpectralDefinitionSurfaceInput(emissionLines = e.toCreateInput.assign)
        }
    }

    implicit class SourceProfileOps(s: SourceProfile) {
      def toInput: SourceProfileInput =
        s match {
          case SourceProfile.Point(definition)          =>
            SourceProfileInput(point = definition.toCreateInput.assign)
          case SourceProfile.Uniform(definition)        =>
            SourceProfileInput(uniform = definition.toCreateInput.assign)
          case SourceProfile.Gaussian(fwhm, definition) =>
            SourceProfileInput(gaussian =
              GaussianInput(fwhm.toInput.assign, definition.toCreateInput.assign).assign
            )
        }
    }

    implicit class GmosNorthSpectropyRowOps(val r: GmosNorthSpectroscopyRow) extends AnyVal {
      def toGmosNITCInput: Input[GmosNITCInput] =
        GmosNITCInput(r.grating, r.fpu.toInput, filter = r.filter.orIgnore).assign
    }

    implicit class GmosSouthSpectropyRowOps(val r: GmosSouthSpectroscopyRow) extends AnyVal {
      def toGmosSITCInput: Input[GmosSITCInput] =
        GmosSITCInput(r.grating, r.fpu.toInput, filter = r.filter.orIgnore).assign
    }

    implicit class RadialVelocityOps(val r: RadialVelocity) extends AnyVal {
      def toITCInput: RadialVelocityInput =
        RadialVelocityInput(metersPerSecond = r.rv.value.assign)
    }

    implicit class ScienceDataOps(val s: ObsQueries.ScienceData) extends AnyVal {
      // From the list of targets selects the ones relevant for ITC
      def itcTargets: List[ITCTarget] = s.targets.asterism
        .map { case TargetWithId(_, target) =>
          targetRV.getOption(target).map(r => ITCTarget(r, Target.sourceProfile.get(target)))
        }
        .flatten
        .hashDistinct

      def baseTarget: Option[Target] =
        NonEmptyList.fromList(s.targets.asterism).map(a => Asterism(a).baseTarget.target)

      def baseSiderealTracking: Option[SiderealTracking] =
        baseTarget match {
          case Some(s: Target.Sidereal) => s.tracking.some
          case _                        => none
        }
    }

    implicit class ITCInstrumentModesOps(val m: InstrumentRow) extends AnyVal {
      def toITCInput: Option[InstrumentModesInput] = m match {
        case r: GmosNorthSpectroscopyRow =>
          InstrumentModesInput(gmosN = r.toGmosNITCInput).some
        case r: GmosSouthSpectroscopyRow =>
          InstrumentModesInput(gmosS = r.toGmosSITCInput).some
        case _                           => none
      }
    }
  }

}
