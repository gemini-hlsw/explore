// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.brightnessesEditor

import crystal.react.*
import explore.*
import explore.utils.IsExpanded
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*

import scala.collection.immutable.SortedMap

private trait BrightnessesEditor[T]:
  def brightnesses: View[SortedMap[Band, BrightnessMeasure[T]]]
  def expanded: View[IsExpanded]
  def disabled: Boolean
