// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Order.*
import cats.data.NonEmptyMap
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
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

sealed abstract class SEDType[T](
  val name:    String,
  val convert: SpectralDefinition[T] => SpectralDefinition[T]
) extends Product
    with Serializable

sealed abstract class SEDTypeEnum[T](
  defaultContinuumUnits: Units Of FluxDensityContinuum[T]
) {
  import UnnormalizedSED.*
  import SpectralDefinition.*

  private def toBandNormalized[T](
    sed: UnnormalizedSED
  ): SpectralDefinition[T] => SpectralDefinition[T] =
    _ match
      case BandNormalized(_, bs) => BandNormalized(sed.some, bs)
      case EmissionLines(_, _)   => BandNormalized(sed.some, SortedMap.empty)

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
            defaultContinuumUnits.withValueTagged(FluxDensityContinuumValue.unsafeFrom(1))
          )
      )
  case object PowerLawType extends BandNormalizedSED("Power Law", PowerLaw(BigDecimal(0)))
  case object BlackBodyType
      extends BandNormalizedSED("Black Body", BlackBody(10000.refined[Positive].withUnit[Kelvin]))
  case object UserDefinedType
      extends BandNormalizedSED(
        "User Defined",
        UserDefined(
          null.asInstanceOf[NonEmptyMap[Wavelength, PosBigDecimal]]
        )
      )

  def fromSpectralDefinition(spectralDefinition: SpectralDefinition[T]): Option[SEDType[T]] =
    spectralDefinition match
      case BandNormalized(Some(StellarLibrary(_)), _)  => StellarLibraryType.some
      case BandNormalized(Some(CoolStarModel(_)), _)   => CoolStarModelType.some
      case BandNormalized(Some(Galaxy(_)), _)          => GalaxyType.some
      case BandNormalized(Some(Planet(_)), _)          => PlanetType.some
      case BandNormalized(Some(Quasar(_)), _)          => QuasarType.some
      case BandNormalized(Some(HIIRegion(_)), _)       => HIIRegionType.some
      case BandNormalized(Some(PlanetaryNebula(_)), _) => PlanetaryNebulaType.some
      case EmissionLines(_, _)                         => EmissionLineType.some
      case BandNormalized(Some(PowerLaw(_)), _)        => PowerLawType.some
      case BandNormalized(Some(BlackBody(_)), _)       => BlackBodyType.some
      case BandNormalized(Some(UserDefined(_)), _)     => UserDefinedType.some
      case BandNormalized(_, _)                        => none

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

object IntegratedSEDType
    extends SEDTypeEnum[Integrated](
      summon[TaggedUnit[ErgsPerSecondCentimeter2Angstrom, FluxDensityContinuum[Integrated]]].unit
    ) {
  given Enumerated[SEDType[Integrated]] = enumSEDType
  given Display[SEDType[Integrated]]    = displaySEDType
}

object SurfaceSEDType
    extends SEDTypeEnum[Surface](
      summon[
        TaggedUnit[ErgsPerSecondCentimeter2AngstromArcsec2, FluxDensityContinuum[Surface]]
      ].unit
    ) {
  given Enumerated[SEDType[Surface]] = enumSEDType
  given Display[SEDType[Surface]]    = displaySEDType
}
