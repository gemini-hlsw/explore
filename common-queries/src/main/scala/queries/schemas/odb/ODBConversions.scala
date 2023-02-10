// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.odb

import clue.data.Input
import clue.data.syntax.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.syntax.*
import lucuma.core.math.dimensional.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.*
import lucuma.core.model.ExposureTimeMode.*
import lucuma.core.model.ProposalClass.*
import lucuma.core.model.*
import lucuma.core.util.*
import lucuma.core.syntax.time.*
import lucuma.schemas.ObservationDB.Enums.PosAngleConstraintMode
import lucuma.schemas.ObservationDB.Types.*

import scala.collection.immutable.SortedMap
import scala.annotation.targetName

// TODO Move to lucuma-schemas
trait ODBConversions:
  extension (id: Observation.Id)
    def toWhereObservation: WhereObservation =
      WhereObservation(id = WhereOrderObservationId(EQ = id.assign).assign)

  extension (ids: List[Observation.Id])
    @targetName("ObservationId_toWhereObservation")
    def toWhereObservation: WhereObservation =
      WhereObservation(id = WhereOrderObservationId(IN = ids.assign).assign)

  extension (id: Program.Id)
    def toWhereProgram: WhereProgram =
      WhereProgram(id = WhereOrderProgramId(EQ = id.assign).assign)

    // @targetName("ProgramId_toWhereObservation")
    // def toWhereObservation: WhereObservation =
    //   WhereObservation()
    //   // WhereObservation(programId = WhereOrderProgramId(EQ = id.assign).assign)

  extension (id: Target.Id)
    def toWhereTarget: WhereTarget =
      WhereTarget(id = WhereOrderTargetId(EQ = id.assign).assign)

  extension (ids: List[Target.Id])
    def toWhereTargets: WhereTarget =
      WhereTarget(OR = ids.map(_.toWhereTarget).assign)

  extension (a: Angle)
    def toInput: AngleInput =
      AngleInput(microarcseconds = a.toMicroarcseconds.assign)

  extension (w: Wavelength)
    def toInput: WavelengthInput =
      WavelengthInput(picometers = w.toPicometers.value.assign)

  extension (info: CatalogInfo)
    def toInput: CatalogInfoInput =
      CatalogInfoInput(info.catalog.assign, info.id.assign, info.objectType.orIgnore)

  extension (ra: RightAscension)
    def toInput: RightAscensionInput =
      RightAscensionInput(microarcseconds = ra.toAngle.toMicroarcseconds.assign)

  extension (dec: Declination)
    def toInput: DeclinationInput =
      DeclinationInput(microarcseconds = dec.toAngle.toMicroarcseconds.assign)

  extension (pm: ProperMotion)
    def toInput: ProperMotionInput =
      ProperMotionInput(
        ra = ProperMotionComponentInput(microarcsecondsPerYear = pm.ra.μasy.value.assign),
        dec = ProperMotionComponentInput(microarcsecondsPerYear = pm.dec.μasy.value.assign)
      )

  extension (rv: RadialVelocity)
    def toInput: RadialVelocityInput =
      RadialVelocityInput(metersPerSecond = rv.rv.value.assign)

  extension (p: Parallax)
    def toInput: ParallaxInput =
      ParallaxInput(microarcseconds = p.μas.value.value.assign)

  extension (u: UnnormalizedSED)
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

  extension (bs: SortedMap[Band, BrightnessMeasure[Integrated]])
    @targetName("IntegratedBrightnessMap_toInput")
    def toInput: List[BandBrightnessIntegratedInput] =
      bs.toList.map { case (band, measure) =>
        BandBrightnessIntegratedInput(
          band = band,
          value = measure.value.value.assign,
          units = Measure.unitsTagged.get(measure).assign,
          error = measure.error.map(_.value).orIgnore
        )
      }

  extension (bs: SortedMap[Band, BrightnessMeasure[Surface]])
    @targetName("SurfaceBrightnessMap_toInput")
    def toInput: List[BandBrightnessSurfaceInput] =
      bs.toList.map { case (band, measure) =>
        BandBrightnessSurfaceInput(
          band = band,
          value = measure.value.value.assign,
          units = Measure.unitsTagged.get(measure).assign,
          error = measure.error.map(_.value).orIgnore
        )
      }

  extension (b: SpectralDefinition.BandNormalized[Integrated])
    def toInput: BandNormalizedIntegratedInput =
      BandNormalizedIntegratedInput(
        sed = b.sed.map(_.toInput).orUnassign,
        brightnesses = b.brightnesses.toInput.assign
      )

  extension (b: SpectralDefinition.BandNormalized[Surface])
    def toInput: BandNormalizedSurfaceInput =
      BandNormalizedSurfaceInput(
        sed = b.sed.map(_.toInput).orUnassign,
        brightnesses = b.brightnesses.toInput.assign
      )

  extension (lines: SortedMap[Wavelength, EmissionLine[Integrated]])
    @targetName("IntegratedEmissionLineMap_toInput")
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

  extension (lines: SortedMap[Wavelength, EmissionLine[Surface]])
    @targetName("SurfaceEmissionLineMap_toInput")
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

  extension (fdc: Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated])
    def toInput: FluxDensityContinuumIntegratedInput = FluxDensityContinuumIntegratedInput(
      value = fdc.value,
      units = Measure.unitsTagged.get(fdc)
    )

  extension (fdc: Measure[PosBigDecimal] Of FluxDensityContinuum[Surface])
    def toInput: FluxDensityContinuumSurfaceInput = FluxDensityContinuumSurfaceInput(
      value = fdc.value,
      units = Measure.unitsTagged.get(fdc)
    )

  extension (e: SpectralDefinition.EmissionLines[Integrated])
    def toInput: EmissionLinesIntegratedInput =
      EmissionLinesIntegratedInput(
        lines = e.lines.toInput.assign,
        fluxDensityContinuum = e.fluxDensityContinuum.toInput.assign
      )

  extension (e: SpectralDefinition.EmissionLines[Surface])
    def toInput: EmissionLinesSurfaceInput =
      EmissionLinesSurfaceInput(
        lines = e.lines.toInput.assign,
        fluxDensityContinuum = e.fluxDensityContinuum.toInput.assign
      )

  extension (s: SpectralDefinition[Integrated])
    def toInput: SpectralDefinitionIntegratedInput =
      s match
        case b @ SpectralDefinition.BandNormalized(_, _) =>
          SpectralDefinitionIntegratedInput(bandNormalized = b.toInput.assign)
        case e @ SpectralDefinition.EmissionLines(_, _)  =>
          SpectralDefinitionIntegratedInput(emissionLines = e.toInput.assign)

  extension (s: SpectralDefinition[Surface])
    def toInput: SpectralDefinitionSurfaceInput =
      s match
        case b @ SpectralDefinition.BandNormalized(_, _) =>
          SpectralDefinitionSurfaceInput(bandNormalized = b.toInput.assign)
        case e @ SpectralDefinition.EmissionLines(_, _)  =>
          SpectralDefinitionSurfaceInput(emissionLines = e.toInput.assign)

  extension (s: SourceProfile)
    def toInput: SourceProfileInput =
      s match
        case SourceProfile.Point(definition)          =>
          SourceProfileInput(point = definition.toInput.assign)
        case SourceProfile.Uniform(definition)        =>
          SourceProfileInput(uniform = definition.toInput.assign)
        case SourceProfile.Gaussian(fwhm, definition) =>
          SourceProfileInput(
            gaussian = GaussianInput(fwhm.toInput.assign, definition.toInput.assign).assign
          )

  extension (p: PosAngleConstraint)
    def toInput: PosAngleConstraintInput =
      p match
        case PosAngleConstraint.Fixed(angle)               =>
          PosAngleConstraintInput(
            mode = PosAngleConstraintMode.Fixed.assign,
            angle = angle.toInput.assign
          )
        case PosAngleConstraint.AllowFlip(angle)           =>
          PosAngleConstraintInput(
            mode = PosAngleConstraintMode.AllowFlip.assign,
            angle = angle.toInput.assign
          )
        case PosAngleConstraint.ParallacticOverride(angle) =>
          PosAngleConstraintInput(
            mode = PosAngleConstraintMode.ParallacticOverride.assign,
            angle = angle.toInput.assign
          )
        case PosAngleConstraint.AverageParallactic         =>
          PosAngleConstraintInput(
            mode = PosAngleConstraintMode.AverageParallactic.assign
          )
        case PosAngleConstraint.Unbounded                  =>
          PosAngleConstraintInput(mode = PosAngleConstraintMode.Unbounded.assign)

  extension (ts: TimeSpan)
    def toInput: TimeSpanInput = TimeSpanInput(microseconds = ts.toMicroseconds.assign)

  extension (etm: ExposureTimeMode)
    def toInput: ExposureTimeModeInput = etm match
      case FixedExposure(count, time) =>
        ExposureTimeModeInput(fixedExposure =
          FixedExposureModeInput(count = count, time = time.toInput).assign
        )
      case SignalToNoise(value)       =>
        ExposureTimeModeInput(signalToNoise = SignalToNoiseModeInput(value = value).assign)

  extension (p: ProposalClass)
    def toInput: ProposalClassInput = p match
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

  extension (proposal: Proposal)
    def toInput: ProposalInput = ProposalInput(
      title = proposal.title.orUnassign,
      proposalClass = proposal.proposalClass.toInput.assign,
      category = proposal.category.orUnassign,
      toOActivation = proposal.toOActivation.assign,
      `abstract` = proposal.abstrakt.orUnassign,
      // Temporary fix until the changes in this issue are complete: https://github.com/gemini-hlsw/lucuma-odb/issues/253
      // partnerSplits = proposal.partnerSplits.toList.map { case (par, pct) =>
      //   PartnerSplitInput(par, pct)
      partnerSplits =
        if (proposal.partnerSplits.isEmpty) Input.unassign
        else
          proposal.partnerSplits.toList.map { case (par, pct) =>
            PartnerSplitInput(par, pct)
          }.assign
    )

  extension (sidereal: Target.Sidereal)
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
        )
      )

  extension (nonsidereal: Target.Nonsidereal)
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
        )
      )

object ODBConversions extends ODBConversions
