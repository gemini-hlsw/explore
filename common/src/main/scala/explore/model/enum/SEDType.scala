// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Order._
import cats.data.NonEmptyMap
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.numeric.Positive
import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units._
import lucuma.core.math.dimensional._
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined._

import scala.collection.immutable.SortedMap

sealed abstract class SEDType[T](
  val name:    String,
  val convert: SpectralDefinition[T] => SpectralDefinition[T]
) extends Product
    with Serializable

sealed abstract class SEDTypeEnum[T](implicit
  enumFDCUnits: Enumerated[Units Of FluxDensityContinuum[T]]
) {
  import UnnormalizedSED._
  import SpectralDefinition._

  private def toBandNormalized[T](
    sed: UnnormalizedSED
  ): SpectralDefinition[T] => SpectralDefinition[T] =
    _ match {
      case BandNormalized(_, bs) => BandNormalized(sed, bs)
      case EmissionLines(_, _)   => BandNormalized(sed, SortedMap.empty)
    }

  protected sealed abstract class BandNormalizedSED(name: String, sed: UnnormalizedSED)
      extends SEDType[T](name, toBandNormalized(sed))

  case object StellarLibraryType
      extends BandNormalizedSED("Stellar Library", StellarLibrary(StellarLibrarySpectrum.O5V))
  case object CoolStarModelType
      extends BandNormalizedSED("Cool Star Model", CoolStarModel(CoolStarTemperature.T400K))
  case object GalaxyType   extends BandNormalizedSED("Galaxy", Galaxy(GalaxySpectrum.Spiral))
  case object PlanetType   extends BandNormalizedSED("Planet", Planet(PlanetSpectrum.Mars))
  case object QuasarType   extends BandNormalizedSED("Quasar", Quasar(QuasarSpectrum.QS0))
  case object HIIRegionType
      extends BandNormalizedSED("HII Region", HIIRegion(HIIRegionSpectrum.OrionNebula))
  case object PlanetaryNebulaType
      extends BandNormalizedSED(
        "Planetary Nebula",
        PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)
      )
  case object EmissionLineType
      extends SEDType[T](
        "Emission Line",
        _ =>
          EmissionLines[T](
            SortedMap.empty,
            enumFDCUnits.all.head.withValueTagged(BigDecimal(1).refined[Positive])
          )
      )
  case object PowerLawType extends BandNormalizedSED("Power Law", PowerLaw(BigDecimal(0)))
  case object BlackBodyType
      extends BandNormalizedSED("Black Body", BlackBody(1000.refined[Positive].withUnit[Kelvin]))
  case object UserDefinedType
      extends BandNormalizedSED(
        "User Defined",
        UserDefined(
          null.asInstanceOf[NonEmptyMap[Wavelength, PosBigDecimal]]
        )
      )

  def fromSpectralDefinition(spectralDefinition: SpectralDefinition[T]): SEDType[T] =
    spectralDefinition match {
      case BandNormalized(StellarLibrary(_), _)  => StellarLibraryType
      case BandNormalized(CoolStarModel(_), _)   => CoolStarModelType
      case BandNormalized(Galaxy(_), _)          => GalaxyType
      case BandNormalized(Planet(_), _)          => PlanetType
      case BandNormalized(Quasar(_), _)          => QuasarType
      case BandNormalized(HIIRegion(_), _)       => HIIRegionType
      case BandNormalized(PlanetaryNebula(_), _) => PlanetaryNebulaType
      case EmissionLines(_, _)                   => EmissionLineType
      case BandNormalized(PowerLaw(_), _)        => PowerLawType
      case BandNormalized(BlackBody(_), _)       => BlackBodyType
      case BandNormalized(UserDefined(_), _)     => UserDefinedType
    }

  protected val enumSEDType: Enumerated[SEDType[T]] =
    Enumerated
      .from[SEDType[T]](
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

  protected val displaySEDType: Display[SEDType[T]] = Display.byShortName(_.name)
}

object IntegratedSEDType extends SEDTypeEnum[Integrated] {
  implicit val enumIntegratedSEDType: Enumerated[SEDType[Integrated]] = enumSEDType
  implicit val displayIntegratedSEDType: Display[SEDType[Integrated]] = displaySEDType
}

object SurfaceSEDType extends SEDTypeEnum[Surface] {
  implicit val enumSurfaceSEDType: Enumerated[SEDType[Surface]] = enumSEDType
  implicit val displaySurfaceSEDType: Display[SEDType[Surface]] = displaySEDType
}
