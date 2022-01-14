// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order
import cats.implicits._
import coulomb.Quantity
import coulomb.cats.implicits._
import coulomb.si.Kelvin
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum.NonStellarLibrarySpectrum
import lucuma.core.enum.StellarLibrarySpectrum

sealed trait SpectralDistribution extends Serializable

object SpectralDistribution {

  /** A black body with a temperature in Kelvin. */
  final case class BlackBody(temperature: Quantity[PosBigDecimal, Kelvin])
      extends SpectralDistribution

  object BlackBody {
    implicit val orderBlackBody: Order[BlackBody] = Order.by(_.temperature)
  }

  /** Defined by power law function. */
  final case class PowerLaw(index: BigDecimal) extends SpectralDistribution

  object PowerLaw {
    implicit val orderPowerLaw: Order[PowerLaw] = Order.by(_.index)
  }

  /** A library defined spectrum. */
  final case class Library(
    librarySpectrum: Either[StellarLibrarySpectrum, NonStellarLibrarySpectrum]
  ) extends SpectralDistribution

  object Library {
    implicit val eqLibrary: Eq[Library] = Eq.by(_.librarySpectrum)
  }
  // TODO: emission line and user-defined

  implicit val eqSpectralDistribution: Eq[SpectralDistribution] = Eq.instance {
    case (a: BlackBody, b: BlackBody) => a === b
    case (a: PowerLaw, b: PowerLaw)   => a === b
    case (a: Library, b: Library)     => a === b
    case _                            => false
  }
}
