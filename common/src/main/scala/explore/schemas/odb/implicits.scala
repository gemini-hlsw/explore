// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import cats.syntax.all._
import clue.data.syntax._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.syntax._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math._
import lucuma.core.math.dimensional._
import lucuma.core.model._
import lucuma.schemas.ObservationDB.Types._

import scala.collection.immutable.SortedMap

import UserPreferencesDB.Types.ExploreResizableWidthInsertInput

// TODO Move to lucuma-schemas
object implicits {

  implicit class AngleOps(val a: Angle) extends AnyVal {
    def toInput: AngleInput =
      AngleInput(microarcseconds = a.toMicroarcseconds.assign)
  }

  implicit class WavelengthOps(val w: Wavelength) extends AnyVal {
    def toInput: WavelengthInput =
      WavelengthInput(picometers = w.toPicometers.value.value.toLong.assign)
  }

  implicit class CatalogInfoOps(val info: CatalogInfo) extends AnyVal {
    def toInput: CatalogInfoInput =
      CatalogInfoInput(info.catalog.assign, info.id.assign, info.objectType.orIgnore)
  }

  implicit class RightAscensionOps(val ra: RightAscension) extends AnyVal {
    def toInput: RightAscensionInput =
      RightAscensionInput(microarcseconds = ra.toAngle.toMicroarcseconds.assign)
  }

  implicit class DeclinationOps(val dec: Declination) extends AnyVal {
    def toInput: DeclinationInput =
      DeclinationInput(microarcseconds = dec.toAngle.toMicroarcseconds.assign)
  }

  implicit class ProperMotionOps(val pm: ProperMotion) extends AnyVal {
    def toInput: ProperMotionInput =
      ProperMotionInput(
        ra = ProperMotionComponentInput(microarcsecondsPerYear = pm.ra.μasy.value.assign),
        dec = ProperMotionComponentInput(microarcsecondsPerYear = pm.dec.μasy.value.assign)
      )
  }

  implicit class RadialVelocityOps(val rv: RadialVelocity) extends AnyVal {
    def toInput: RadialVelocityInput =
      RadialVelocityInput(metersPerSecond = rv.rv.value.assign)
  }

  implicit class ParallaxOps(val p: Parallax) extends AnyVal {
    def toInput: ParallaxModelInput =
      ParallaxModelInput(microarcseconds = p.μas.value.value.assign)
  }

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
          UnnormalizedSedInput(blackBodyTempK = BigDecimal(temperature.value.value).assign)
        case UnnormalizedSED.UserDefined(fluxDensities)               =>
          UnnormalizedSedInput(fluxDensities = fluxDensities.toSortedMap.toList.map {
            case (wavelength, value) => FluxDensity(wavelength.toInput, value.value)
          }.assign)
      }
  }

  implicit class IntegratedBandBrightnessesOps(
    val bs: SortedMap[Band, BrightnessMeasure[Integrated]]
  ) extends AnyVal {
    def toInput: List[BandBrightnessIntegratedInput] =
      bs.toList.map { case (band, measure) =>
        BandBrightnessIntegratedInput(
          band = band,
          value = BrightnessValue.fromBigDecimal.reverseGet(measure.value).assign,
          units = Measure.unitsTagged.get(measure).assign,
          error = measure.error.map(BrightnessValue.fromBigDecimal.reverseGet).orIgnore
        )
      }
  }

  implicit class SurfaceBandBrightnessesOps(
    val bs: SortedMap[Band, BrightnessMeasure[Surface]]
  ) extends AnyVal {
    def toInput: List[BandBrightnessSurfaceInput] =
      bs.toList.map { case (band, measure) =>
        BandBrightnessSurfaceInput(
          band = band,
          value = BrightnessValue.fromBigDecimal.reverseGet(measure.value).assign,
          units = Measure.unitsTagged.get(measure).assign,
          error = measure.error.map(BrightnessValue.fromBigDecimal.reverseGet).orIgnore
        )
      }
  }

  implicit class IntegratedBandNormalizedOps(val b: SpectralDefinition.BandNormalized[Integrated])
      extends AnyVal {
    def toInput: BandNormalizedIntegratedInput =
      BandNormalizedIntegratedInput(
        sed = b.sed.toInput.assign,
        brightnesses = b.brightnesses.toInput.assign
      )
  }

  implicit class SurfaceBandNormalizedOps(val b: SpectralDefinition.BandNormalized[Surface])
      extends AnyVal {
    def toInput: BandNormalizedSurfaceInput =
      BandNormalizedSurfaceInput(
        sed = b.sed.toInput.assign,
        brightnesses = b.brightnesses.toInput.assign
      )
  }

  implicit class IntegratedEmissionLineMapOps(
    val lines: SortedMap[Wavelength, EmissionLine[Integrated]]
  ) extends AnyVal {
    def toInput: List[EmissionLineIntegratedInput] =
      lines.toList.map { case (wavelength, line) =>
        EmissionLineIntegratedInput(
          wavelength = wavelength.toInput,
          lineWidth = line.lineWidth.value.value.assign,
          lineFlux = LineFluxIntegratedInput(
            line.lineFlux.value.value,
            Measure.unitsTagged.get(line.lineFlux)
          ).assign
        )
      }
  }

  implicit class SurfaceEmissionLineMapOps(
    val lines: SortedMap[Wavelength, EmissionLine[Surface]]
  ) extends AnyVal {
    def toInput: List[EmissionLineSurfaceInput] =
      lines.toList.map { case (wavelength, line) =>
        EmissionLineSurfaceInput(
          wavelength = wavelength.toInput,
          lineWidth = line.lineWidth.value.value.assign,
          lineFlux = LineFluxSurfaceInput(
            line.lineFlux.value.value,
            Measure.unitsTagged.get(line.lineFlux)
          ).assign
        )
      }
  }

  implicit class IntegratedFluxDensityContinuumOps(
    val fdc: Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]
  ) extends AnyVal {
    def toInput: FluxDensityContinuumIntegratedInput = FluxDensityContinuumIntegratedInput(
      value = fdc.value.value,
      units = Measure.unitsTagged.get(fdc)
    )
  }

  implicit class SurfaceFluxDensityContinuumOps(
    val fdc: Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]
  ) extends AnyVal {
    def toInput: FluxDensityContinuumSurfaceInput = FluxDensityContinuumSurfaceInput(
      value = fdc.value.value,
      units = Measure.unitsTagged.get(fdc)
    )
  }

  implicit class IntegratedEmissionLinesOps(val e: SpectralDefinition.EmissionLines[Integrated])
      extends AnyVal {
    def toInput: EmissionLinesIntegratedInput =
      EmissionLinesIntegratedInput(
        lines = e.lines.toInput.assign,
        fluxDensityContinuum = e.fluxDensityContinuum.toInput.assign
      )
  }

  implicit class SurfaceEmissionLinesOps(val e: SpectralDefinition.EmissionLines[Surface])
      extends AnyVal {
    def toInput: EmissionLinesSurfaceInput =
      EmissionLinesSurfaceInput(
        lines = e.lines.toInput.assign,
        fluxDensityContinuum = e.fluxDensityContinuum.toInput.assign
      )
  }

  implicit class IntegratedSpectralDefinitionOps(val s: SpectralDefinition[Integrated])
      extends AnyVal {
    def toInput: SpectralDefinitionIntegratedInput =
      s match {
        case b @ SpectralDefinition.BandNormalized(_, _) =>
          SpectralDefinitionIntegratedInput(bandNormalized = b.toInput.assign)
        case e @ SpectralDefinition.EmissionLines(_, _)  =>
          SpectralDefinitionIntegratedInput(emissionLines = e.toInput.assign)
      }
  }

  implicit class SurfaceSpectralDefinitionOps(val s: SpectralDefinition[Surface]) extends AnyVal {
    def toInput: SpectralDefinitionSurfaceInput =
      s match {
        case b @ SpectralDefinition.BandNormalized(_, _) =>
          SpectralDefinitionSurfaceInput(bandNormalized = b.toInput.assign)
        case e @ SpectralDefinition.EmissionLines(_, _)  =>
          SpectralDefinitionSurfaceInput(emissionLines = e.toInput.assign)
      }
  }

  implicit class SourceProfileOps(val s: SourceProfile) extends AnyVal {
    def toInput: SourceProfileInput =
      s match {
        case SourceProfile.Point(definition)          =>
          SourceProfileInput(point = definition.toInput.assign)
        case SourceProfile.Uniform(definition)        =>
          SourceProfileInput(uniform = definition.toInput.assign)
        case SourceProfile.Gaussian(fwhm, definition) =>
          SourceProfileInput(gaussian =
            GaussianInput(fwhm.toInput.assign, definition.toInput.assign).assign
          )
      }
  }

  implicit class SiderealTargetOps(val sidereal: Target.Sidereal) extends AnyVal {
    def toInput: SiderealInput = SiderealInput(
      ra = sidereal.tracking.baseCoordinates.ra.toInput.assign,
      dec = sidereal.tracking.baseCoordinates.dec.toInput.assign,
      epoch = Epoch.fromString.reverseGet(sidereal.tracking.epoch).assign,
      properMotion = sidereal.tracking.properMotion.map(_.toInput).orIgnore,
      radialVelocity = sidereal.tracking.radialVelocity.map(_.toInput).orIgnore,
      parallax = sidereal.tracking.parallax.map(_.toInput).orIgnore,
      catalogInfo = sidereal.catalogInfo.map(_.toInput).orIgnore
    )

    def toCreateTargetInput(id: Option[Target.Id] = none): CreateTargetInput =
      CreateTargetInput(
        targetId = id.orIgnore,
        name = sidereal.name,
        sidereal = toInput.assign,
        sourceProfile = sidereal.sourceProfile.toInput
      )
  }

  implicit class NonsiderealTargetOps(val nonsidereal: Target.Nonsidereal) extends AnyVal {
    def toInput: NonsiderealInput = NonsiderealInput(
      key = NonEmptyString.unsafeFrom(nonsidereal.ephemerisKey.asJson.toString).assign
    )

    def toCreateTargetInput(id: Option[Target.Id] = none): CreateTargetInput =
      CreateTargetInput(
        targetId = id.orIgnore,
        name = nonsidereal.name,
        nonsidereal = toInput.assign,
        sourceProfile = nonsidereal.sourceProfile.toInput
      )
  }

  implicit def widthUpsertInput(w: WidthUpsertInput): ExploreResizableWidthInsertInput =
    ExploreResizableWidthInsertInput(
      w.section.value.assign,
      w.user.toString.assign,
      w.width.assign
    )
}
