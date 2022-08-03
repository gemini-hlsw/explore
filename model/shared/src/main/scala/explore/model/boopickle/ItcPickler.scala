// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic._
import coulomb.Quantity
import cats.data.NonEmptyMap
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.modes.InstrumentRow
import explore.modes.ModeAO
import explore.modes.ModeSlitSize
import explore.modes.ModeWavelength
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import explore.modes._
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.units._
import explore.model.itc.ItcTarget
import lucuma.core.math.RadialVelocity
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.dimensional._
import lucuma.core.model.UnnormalizedSED
import lucuma.core.math.BrightnessUnits.Brightness
import lucuma.core.math.BrightnessUnits.LineFlux
import lucuma.core.math.BrightnessUnits.FluxDensityContinuum
import lucuma.core.math.Wavelength
import lucuma.core.model.EmissionLine

// Boopicklers for itc related types
trait ItcPicklers extends CommonPicklers {

  implicit val gmosNPickler: Pickler[GmosNorthSpectroscopyRow] =
    transformPickler(Function.tupled(GmosNorthSpectroscopyRow.apply _))(x =>
      (x.grating, x.fpu, x.filter)
    )

  implicit val gmosSPickler: Pickler[GmosSouthSpectroscopyRow] =
    transformPickler(Function.tupled(GmosSouthSpectroscopyRow.apply _))(x =>
      (x.grating, x.fpu, x.filter)
    )

  implicit val f2Pickler: Pickler[Flamingos2SpectroscopyRow] =
    transformPickler(Function.tupled(Flamingos2SpectroscopyRow.apply _))(x => (x.grating, x.filter))

  implicit val gpiPickler: Pickler[GpiSpectroscopyRow] =
    transformPickler(Function.tupled(GpiSpectroscopyRow.apply _))(x => (x.grating, x.filter))

  implicit val gnirsPickler: Pickler[GnirsSpectroscopyRow] =
    transformPickler(Function.tupled(GnirsSpectroscopyRow.apply _))(x => (x.grating, x.filter))

  implicit val genericRowPickler: Pickler[GenericSpectroscopyRow] =
    transformPickler(Function.tupled(GenericSpectroscopyRow.apply _))(x =>
      (x.i, x.grating, x.filter)
    )

  implicit val instRowPickler: Pickler[InstrumentRow] =
    compositePickler[InstrumentRow]
      .addConcreteType[GmosNorthSpectroscopyRow]
      .addConcreteType[GmosSouthSpectroscopyRow]
      .addConcreteType[Flamingos2SpectroscopyRow]
      .addConcreteType[GpiSpectroscopyRow]
      .addConcreteType[GnirsSpectroscopyRow]
      .addConcreteType[GenericSpectroscopyRow]

  implicit val mwPickler: Pickler[ModeWavelength] =
    transformPickler(ModeWavelength.apply)(_.w)

  implicit val msPickler: Pickler[ModeSlitSize] =
    transformPickler(ModeSlitSize.apply)(_.size)

  implicit val rowPickler: Pickler[SpectroscopyModeRow] =
    transformPickler(
      (x: Tuple13[
        Int,
        InstrumentRow,
        NonEmptyString,
        FocalPlane,
        Option[SpectroscopyCapabilities],
        ModeAO,
        ModeWavelength,
        ModeWavelength,
        ModeWavelength,
        Quantity[NonNegBigDecimal, Micrometer],
        PosInt,
        ModeSlitSize,
        ModeSlitSize
      ]) =>
        x match {
          case (
                id,
                instrument,
                config,
                focalPlane,
                capabilities,
                ao,
                minWavelength,
                maxWavelength,
                optimalWavelength,
                wavelengthCoverage,
                resolution,
                slitLength,
                slitWidth
              ) =>
            SpectroscopyModeRow(id,
                                instrument,
                                config,
                                focalPlane,
                                capabilities,
                                ao,
                                minWavelength,
                                maxWavelength,
                                optimalWavelength,
                                wavelengthCoverage,
                                resolution,
                                slitLength,
                                slitWidth
            )
        }
    )(x =>
      (x.id,
       x.instrument,
       x.config,
       x.focalPlane,
       x.capabilities,
       x.ao,
       x.minWavelength,
       x.maxWavelength,
       x.optimalWavelength,
       x.wavelengthCoverage,
       x.resolution,
       x.slitLength,
       x.slitWidth
      )
    )

  given Pickler[SpectroscopyModesMatrix] = generatePickler

  given Pickler[UnnormalizedSED.StellarLibrary] = generatePickler

  given Pickler[UnnormalizedSED.CoolStarModel] =
    transformPickler(UnnormalizedSED.CoolStarModel.apply)(_.temperature)

  implicit val sedGalaxyPickler: Pickler[UnnormalizedSED.Galaxy] =
    transformPickler(UnnormalizedSED.Galaxy.apply)(_.galaxySpectrum)

  implicit val sedPlanetPickler: Pickler[UnnormalizedSED.Planet] =
    transformPickler(UnnormalizedSED.Planet.apply)(_.planetSpectrum)

  implicit val sedPQuasarPickler: Pickler[UnnormalizedSED.Quasar] =
    transformPickler(UnnormalizedSED.Quasar.apply)(_.quasarSpectrum)

  implicit val sedHIIRegionPickler: Pickler[UnnormalizedSED.HIIRegion] =
    transformPickler(UnnormalizedSED.HIIRegion.apply)(_.hiiRegionSpectrum)

  implicit val sedPNPickler: Pickler[UnnormalizedSED.PlanetaryNebula] =
    transformPickler(UnnormalizedSED.PlanetaryNebula.apply)(_.planetaryNebulaSpectrum)

  implicit val sedPLPNPickler: Pickler[UnnormalizedSED.PowerLaw] =
    transformPickler(UnnormalizedSED.PowerLaw.apply)(_.index)

  implicit val sedBBPickler: Pickler[UnnormalizedSED.BlackBody] =
    transformPickler(UnnormalizedSED.BlackBody.apply)(_.temperature)

  implicit val sedUDPickler: Pickler[UnnormalizedSED.UserDefined] =
    transformPickler(UnnormalizedSED.UserDefined.apply)(_.fluxDensities)

  implicit def sedPickler[A]: Pickler[UnnormalizedSED] =
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
    transformPickler((x: (Units Of T, N)) => x._1.withValueTagged(x._2))(x =>
      (Measure.unitsTagged.get(x), x.value)
    )

  // given brightessMeasurePickler[A]: Pickler[BrightnessUnits.BrightnessMeasure[A]] =
  //   ???
  //   ???
  // transformPickler(Function.tupled(Measure.apply[A] _))(x => (x.value, x.units, x.error))

  given bandNormalizerPickler[A](using
    Pickler[Units Of Brightness[A]]
  ): Pickler[SpectralDefinition.BandNormalized[A]] =
    transformPickler(Function.tupled(SpectralDefinition.BandNormalized.apply[A] _))(x =>
      (x.sed, x.brightnesses)
    )

  given bandNormalizerPickler[A](using
    Pickler[Units Of LineFlux[A]],
    Pickler[Units Of FluxDensityContinuum[A]]
  ): Pickler[SpectralDefinition.EmissionLines[A]] =
    transformPickler((x: (List[(Wavelength, EmissionLine[A])], FluxDensityContinuum[A])) =>
      SpectralDefinition.EmissionLines(x._1, x._2)
    )(x => (x.lines.toList, x.fluxDensityContinuum))

  given spectralDefinitionPickler[A](using
    Pickler[Units Of Brightness[A]]
  ): Pickler[SpectralDefinition[A]] =
    compositePickler[SpectralDefinition[A]]
      .addConcreteType[SpectralDefinition.BandNormalized[A]]
      .addConcreteType[SpectralDefinition.EmissionLines[A]]
  //
  // implicit val sourceProfilePointPickler: Pickler[SourceProfile.Point] =
  //   transformPickler(SourceProfile.Point.apply)(_.spectralDefinition)
  //
  // implicit val sourceProfilePickler: Pickler[SourceProfile] =
  //   compositePickler[SourceProfile]
  //     .addConcreteType[SourceProfile.Point]
  //     .addConcreteType[SourceProfile.Uniform]
  //     .addConcreteType[SourceProfile.Gaussian]
  //
  // implicit val picklerItcTarge: Pickler[ItcTarget] =
  //   transformPickler(Function.tupled(ItcTarget.apply _))(x => (x.profile, x.rv))
}

object ItcPicklers extends ItcPicklers
