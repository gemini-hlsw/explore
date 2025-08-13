// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.emissionLineEditor

import crystal.react.*
import explore.*
import explore.model.enums.WavelengthUnits
import explore.utils.IsExpanded
import japgolly.scalajs.react.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.util.Of
import lucuma.react.common.ReactFnProps

import scala.collection.immutable.SortedMap

case class IntegratedEmissionLineEditor(
  emissionLines: View[SortedMap[Wavelength, EmissionLine[Integrated]]],
  expanded:      View[IsExpanded],
  disabled:      Boolean,
  units:         WavelengthUnits
) extends ReactFnProps[IntegratedEmissionLineEditor](IntegratedEmissionLineEditor.component)
    with EmissionLineEditor[Integrated]

object IntegratedEmissionLineEditor
    extends EmissionLineEditorBuilder[Integrated, IntegratedEmissionLineEditor] {
  val defaultLineUnits =
    summon[TaggedUnit[ErgsPerSecondCentimeter2, LineFlux[Integrated]]].unit

  val component = ScalaFnComponent[IntegratedEmissionLineEditor]: props =>
    componentWithWavelengthUnits(props.units)(props)
}
