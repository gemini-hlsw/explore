// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Order.*
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import eu.timepit.refined.numeric.Positive
import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.dimensional.Units.*
import lucuma.core.math.units.*
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.*
import lucuma.refined.*

import scala.collection.immutable.SortedMap

sealed abstract class SedType[T](val name: String) extends Product with Serializable

object SedType {
  sealed abstract class Immediate[T](
    name:        String,
    val convert: SpectralDefinition[T] => SpectralDefinition[T]
  ) extends SedType[T](name)

  object Immediate {
    def unapply[T](sed: Immediate[T]) =
      (sed.name, sed.convert)
  }
}

sealed abstract class SedTypeEnum[T](
  defaultContinuumUnits: Units Of FluxDensityContinuum[T]
) {
  import UnnormalizedSED.*
  import SpectralDefinition.*

  def toBandNormalized[T](
    sed: UnnormalizedSED
  ): SpectralDefinition[T] => SpectralDefinition[T] =
    _ match
      case BandNormalized(_, bs) => BandNormalized(sed.some, bs)
      case EmissionLines(_, _)   => BandNormalized(sed.some, SortedMap.empty)

  protected object BandNormalizedSed {
    sealed abstract class Immediate(
      name: String,
      sed:  UnnormalizedSED
    ) extends SedType.Immediate[T](name, toBandNormalized(sed))
  }

  case object StellarLibraryType
      extends BandNormalizedSed.Immediate(
        "Stellar Library",
        StellarLibrary(StellarLibrarySpectrum.O5V)
      )
  case object CoolStarModelType
      extends BandNormalizedSed.Immediate(
        "Cool Star Model",
        CoolStarModel(CoolStarTemperature.T400K)
      )
  case object GalaxyType
      extends BandNormalizedSed.Immediate("Galaxy", Galaxy(GalaxySpectrum.Spiral))
  case object PlanetType      extends BandNormalizedSed.Immediate("Planet", Planet(PlanetSpectrum.Mars))
  case object QuasarType      extends BandNormalizedSed.Immediate("Quasar", Quasar(QuasarSpectrum.QS0))
  case object HIIRegionType
      extends BandNormalizedSed.Immediate("HII Region", HIIRegion(HIIRegionSpectrum.OrionNebula))
  case object PlanetaryNebulaType
      extends BandNormalizedSed.Immediate(
        "Planetary Nebula",
        PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)
      )
  case object EmissionLineType
      extends SedType.Immediate[T](
        "Emission Line",
        _ =>
          EmissionLines[T](
            SortedMap.empty,
            defaultContinuumUnits.withValueTagged(FluxDensityContinuumValue.unsafeFrom(1))
          )
      )
  case object PowerLawType    extends BandNormalizedSed.Immediate("Power Law", PowerLaw(BigDecimal(0)))
  case object BlackBodyType
      extends BandNormalizedSed.Immediate(
        "Black Body",
        BlackBody(10000.refined[Positive].withUnit[Kelvin])
      )
  case object UserDefinedType extends SedType[T]("User Defined")

  def fromSpectralDefinition(spectralDefinition: SpectralDefinition[T]): Option[SedType[T]] =
    spectralDefinition match
      case BandNormalized(Some(StellarLibrary(_)), _)        => StellarLibraryType.some
      case BandNormalized(Some(CoolStarModel(_)), _)         => CoolStarModelType.some
      case BandNormalized(Some(Galaxy(_)), _)                => GalaxyType.some
      case BandNormalized(Some(Planet(_)), _)                => PlanetType.some
      case BandNormalized(Some(Quasar(_)), _)                => QuasarType.some
      case BandNormalized(Some(HIIRegion(_)), _)             => HIIRegionType.some
      case BandNormalized(Some(PlanetaryNebula(_)), _)       => PlanetaryNebulaType.some
      case EmissionLines(_, _)                               => EmissionLineType.some
      case BandNormalized(Some(PowerLaw(_)), _)              => PowerLawType.some
      case BandNormalized(Some(BlackBody(_)), _)             => BlackBodyType.some
      case BandNormalized(Some(UserDefinedAttachment(_)), _) => UserDefinedType.some
      case BandNormalized(_, _)                              => none

  given Enumerated[SedType[T]] =
    Enumerated
      .from[SedType[T]](
        StellarLibraryType,
        CoolStarModelType,
        GalaxyType,
        PlanetType,
        QuasarType,
        HIIRegionType,
        PlanetaryNebulaType,
        EmissionLineType,
        PowerLawType,
        BlackBodyType,
        UserDefinedType
      )
      .withTag(_.name)

  given Display[SedType[T]] = Display.byShortName(_.name)
}

object IntegratedSedType
    extends SedTypeEnum[Integrated](
      summon[TaggedUnit[ErgsPerSecondCentimeter2Angstrom, FluxDensityContinuum[Integrated]]].unit
    )

object SurfaceSedType
    extends SedTypeEnum[Surface](
      summon[
        TaggedUnit[ErgsPerSecondCentimeter2AngstromArcsec2, FluxDensityContinuum[Surface]]
      ].unit
    )
