// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic._
import coulomb.Quantity
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
                id: Int, // Give them a local id to simplify reusability
                instrument: InstrumentRow,
                config: NonEmptyString,
                focalPlane: FocalPlane,
                capabilities: Option[SpectroscopyCapabilities],
                ao: ModeAO,
                minWavelength: ModeWavelength,
                maxWavelength: ModeWavelength,
                optimalWavelength: ModeWavelength,
                wavelengthCoverage: Quantity[NonNegBigDecimal, Micrometer],
                resolution: PosInt,
                slitLength: ModeSlitSize,
                slitWidth: ModeSlitSize
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

  implicit val matrixPickler: Pickler[SpectroscopyModesMatrix] =
    transformPickler(SpectroscopyModesMatrix.apply)(_.matrix)
}

object ItcPicklers extends ItcPicklers
