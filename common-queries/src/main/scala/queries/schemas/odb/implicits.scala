// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ResizableSection
import io.circe.syntax._
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math._
import lucuma.core.math.dimensional._
import lucuma.core.model.ExposureTimeMode._
import lucuma.core.model.ProposalClass._
import lucuma.core.model._
import lucuma.core.syntax.time._
import lucuma.schemas.ObservationDB.Enums.PosAngleConstraintType
import lucuma.schemas.ObservationDB.Types._
import queries.schemas.UserPreferencesDB.Types.ExploreResizableWidthInsertInput

import scala.collection.immutable.SortedMap

final case class WidthUpsertInput(user: User.Id, section: ResizableSection, width: Int)

// TODO Move to lucuma-schemas
object implicits {
  implicit class ObsIdOps(val id: Observation.Id) extends AnyVal {
    def toWhereObservation: WhereObservation =
      WhereObservation(id = WhereOrderObservationId(EQ = id.assign).assign)
  }

  implicit class ObsIdListOps(val ids: List[Observation.Id]) extends AnyVal {
    def toWhereObservation: WhereObservation =
      WhereObservation(id = WhereOrderObservationId(IN = ids.assign).assign)
  }

  implicit class ProgramidOps(val id: Program.Id) extends AnyVal {
    def toWhereProgram: WhereProgram =
      WhereProgram(id = WhereOrderProgramId(EQ = id.assign).assign)

    def toWhereObservation: WhereObservation =
      WhereObservation(programId = WhereOrderProgramId(EQ = id.assign).assign)
  }

  implicit class TargetIdOps(val id: Target.Id) extends AnyVal {
    def toWhereTarget: WhereTarget =
      WhereTarget(id = WhereOrderTargetId(EQ = id.assign).assign)
  }

  implicit class AngleOps(val a: Angle) extends AnyVal {
    def toInput: AngleInput =
      AngleInput(microarcseconds = a.toMicroarcseconds.assign)
  }

  implicit class WavelengthOps(val w: Wavelength) extends AnyVal {
    def toInput: WavelengthInput =
      WavelengthInput(picometers = w.toPicometers.value.assign)
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
    def toInput: ParallaxInput =
      ParallaxInput(microarcseconds = p.μas.value.value.assign)
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
          UnnormalizedSedInput(blackBodyTempK = temperature.value.assign)
        case UnnormalizedSED.UserDefined(fluxDensities)               =>
          UnnormalizedSedInput(fluxDensities = fluxDensities.toSortedMap.toList.map {
            case (wavelength, value) => FluxDensity(wavelength.toInput, value)
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
          value = measure.value.assign,
          units = Measure.unitsTagged.get(measure).assign,
          error = measure.error.orIgnore
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
          value = measure.value.assign,
          units = Measure.unitsTagged.get(measure).assign,
          error = measure.error.orIgnore
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
          lineWidth = line.lineWidth.value.assign,
          lineFlux = LineFluxIntegratedInput(
            line.lineFlux.value,
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
          lineWidth = line.lineWidth.value.assign,
          lineFlux = LineFluxSurfaceInput(
            line.lineFlux.value,
            Measure.unitsTagged.get(line.lineFlux)
          ).assign
        )
      }
  }

  implicit class IntegratedFluxDensityContinuumOps(
    val fdc: Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]
  ) extends AnyVal {
    def toInput: FluxDensityContinuumIntegratedInput = FluxDensityContinuumIntegratedInput(
      value = fdc.value,
      units = Measure.unitsTagged.get(fdc)
    )
  }

  implicit class SurfaceFluxDensityContinuumOps(
    val fdc: Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]
  ) extends AnyVal {
    def toInput: FluxDensityContinuumSurfaceInput = FluxDensityContinuumSurfaceInput(
      value = fdc.value,
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

  implicit class PosAngleConstraintOps(val p: PosAngleConstraint) extends AnyVal {
    def toInput: PosAngleConstraintInput =
      p match {
        case PosAngleConstraint.Fixed(angle)               =>
          PosAngleConstraintInput(constraint = PosAngleConstraintType.Fixed.assign,
                                  angle = angle.toInput.assign
          )
        case PosAngleConstraint.AllowFlip(angle)           =>
          PosAngleConstraintInput(constraint = PosAngleConstraintType.AllowFlip.assign,
                                  angle = angle.toInput.assign
          )
        case PosAngleConstraint.ParallacticOverride(angle) =>
          PosAngleConstraintInput(constraint = PosAngleConstraintType.AverageParallactic.assign,
                                  angle = angle.toInput.assign
          )
        case PosAngleConstraint.AverageParallactic         =>
          PosAngleConstraintInput(constraint = PosAngleConstraintType.AverageParallactic.assign,
                                  angle = Input.unassign
          )
      }
  }

  extension(nnd: NonNegDuration)
    def toInput: NonNegDurationInput =
      NonNegDurationInput(microseconds = PosLong.unsafeFrom(nnd.value.toMicros).assign)

  implicit class ExposureTimeModeOps(val etm: ExposureTimeMode) extends AnyVal {
    def toInput: ExposureTimeModeInput = etm match {
      case FixedExposure(count, time) =>
        ExposureTimeModeInput(fixedExposure =
          FixedExposureModeInput(count = count, time = time.toInput).assign
        )
      case SignalToNoise(value)       =>
        ExposureTimeModeInput(signalToNoise = SignalToNoiseModeInput(value = value).assign)
    }
  }

  implicit class PropocalClassOps(val p: ProposalClass) extends AnyVal {
    def toInput: ProposalClassInput = p match {
      case DemoScience(minPercentTime)                                  =>
        ProposalClassInput(demoScience =
          DemoScienceInput(minPercentTime = minPercentTime.assign).assign
        )
      case Exchange(minPercentTime)                                     =>
        ProposalClassInput(exchange = ExchangeInput(minPercentTime = minPercentTime.assign).assign)
      case LargeProgram(minPercentTime, minPercentTotalTime, totalTime) =>
        ProposalClassInput(largeProgram =
          LargeProgramInput(
            minPercentTime = minPercentTime.assign,
            minPercentTotalTime = minPercentTotalTime.assign,
            totalTime = totalTime.toInput.assign
          ).assign
        )
      case Queue(minPercentTime)                                        =>
        ProposalClassInput(queue = QueueInput(minPercentTime = minPercentTime.assign).assign)
      case FastTurnaround(minPercentTime)                               =>
        ProposalClassInput(fastTurnaround =
          FastTurnaroundInput(minPercentTime = minPercentTime.assign).assign
        )
      case DirectorsTime(minPercentTime)                                =>
        ProposalClassInput(directorsTime =
          DirectorsTimeInput(minPercentTime = minPercentTime.assign).assign
        )
      case Intensive(minPercentTime, minPercentTotalTime, totalTime)    =>
        ProposalClassInput(intensive =
          IntensiveInput(
            minPercentTime = minPercentTime.assign,
            minPercentTotalTime = minPercentTotalTime.assign,
            totalTime = totalTime.toInput.assign
          ).assign
        )
      case SystemVerification(minPercentTime)                           =>
        ProposalClassInput(systemVerification =
          SystemVerificationInput(minPercentTime = minPercentTime.assign).assign
        )
      case Classical(minPercentTime)                                    =>
        ProposalClassInput(classical =
          ClassicalInput(minPercentTime = minPercentTime.assign).assign
        )
      case PoorWeather(minPercentTime)                                  =>
        ProposalClassInput(poorWeather =
          PoorWeatherInput(minPercentTime = minPercentTime.assign).assign
        )
    }
  }

  implicit class ProposalOps(val proposal: Proposal) extends AnyVal {
    def toInput: ProposalInput = ProposalInput(
      title = proposal.title.orUnassign,
      proposalClass = proposal.proposalClass.toInput.assign,
      category = proposal.category.orUnassign,
      toOActivation = proposal.toOActivation.assign,
      `abstract` = proposal.abstrakt.orUnassign,
      partnerSplits = proposal.partnerSplits.toList.map { case (par, pct) =>
        PartnerSplitsInput(par.assign, pct.assign)
      }.assign
    )
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

    def toCreateTargetInput(programId: Program.Id): CreateTargetInput =
      CreateTargetInput(
        programId = programId,
        SET = TargetPropertiesInput(
          name = sidereal.name.assign,
          sidereal = toInput.assign,
          sourceProfile = sidereal.sourceProfile.toInput.assign
        ).assign
      )
  }

  implicit class NonsiderealTargetOps(val nonsidereal: Target.Nonsidereal) extends AnyVal {
    def toInput: NonsiderealInput = NonsiderealInput(
      key = NonEmptyString.unsafeFrom(nonsidereal.ephemerisKey.asJson.toString).assign
    )

    def toCreateTargetInput(programId: Program.Id): CreateTargetInput =
      CreateTargetInput(
        programId = programId,
        SET = TargetPropertiesInput(
          name = nonsidereal.name.assign,
          nonsidereal = toInput.assign,
          sourceProfile = nonsidereal.sourceProfile.toInput.assign
        ).assign
      )
  }

  implicit def widthUpsertInput(w: WidthUpsertInput): ExploreResizableWidthInsertInput =
    ExploreResizableWidthInsertInput(
      w.section.value.assign,
      w.user.toString.assign,
      w.width.assign
    )
}
