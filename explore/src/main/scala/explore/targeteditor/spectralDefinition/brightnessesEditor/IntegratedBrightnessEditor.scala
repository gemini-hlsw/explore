// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.brightnessesEditor

import crystal.react.*
import explore.*
import explore.utils.IsExpanded
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.dimensional.*
import lucuma.core.util.Of
import lucuma.react.common.ReactFnProps

import scala.collection.immutable.SortedMap

case class IntegratedBrightnessEditor(
  brightnesses: View[SortedMap[Band, BrightnessMeasure[Integrated]]],
  expanded:     View[IsExpanded],
  disabled:     Boolean
) extends ReactFnProps[IntegratedBrightnessEditor](IntegratedBrightnessEditor.component)
    with BrightnessesEditor[Integrated]

object IntegratedBrightnessEditor
    extends BrightnessesEditorBuilder[Integrated, IntegratedBrightnessEditor]:
  protected val label                                                          = "Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Integrated] =
    _.defaultIntegrated.units
