// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import coulomb.*
import coulomb.units.si.Kelvin
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string
import explore.*
import explore.common.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.AppContext
import explore.model.display.given
import explore.model.enums.IntegratedSEDType
import explore.model.enums.IntegratedSEDType.given
import explore.model.enums.SedType
import explore.model.enums.SurfaceSEDType
import explore.model.enums.SurfaceSEDType.given
import explore.model.syntax.all.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.CatalogName
import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.FluxDensityContinuumValueRefinement
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.PrimeStyles
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.typelevel.log4cats.Logger
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem

import scala.collection.immutable.HashSet
import scala.collection.immutable.SortedMap

private trait SpectralDefinitionEditor[T, S]:
  def spectralDefinition: Aligner[SpectralDefinition[T], S]
  def catalogInfo: Option[CatalogInfo]
  def calibrationRole: Option[CalibrationRole]
  def brightnessExpanded: View[IsExpanded]
  def disabled: Boolean

  def toInput: SpectralDefinition[T] => S
  def sedAlignerOpt: Option[Aligner[UnnormalizedSED, UnnormalizedSedInput]]
  def bandBrightnessesViewOpt: Option[View[SortedMap[Band, BrightnessMeasure[T]]]]
  def emissionLinesViewOpt: Option[View[SortedMap[Wavelength, EmissionLine[T]]]]
  def fluxDensityContinuumOpt: Option[View[FluxDensityContinuumMeasure[T]]]
