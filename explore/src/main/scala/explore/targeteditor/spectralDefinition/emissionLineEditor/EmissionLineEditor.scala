// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.emissionLineEditor

import crystal.react.*
import explore.*
import explore.utils.IsExpanded
import lucuma.core.math.Wavelength
import lucuma.core.model.EmissionLine

import scala.collection.immutable.SortedMap

private trait EmissionLineEditor[T]:
  def emissionLines: View[SortedMap[Wavelength, EmissionLine[T]]]
  def expanded: View[IsExpanded]
  def disabled: Boolean
