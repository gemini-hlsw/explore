// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.brightnessesEditor

import cats.Order.*
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators
import explore.model.display.given
import explore.utils.IsExpanded
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional.*
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Panel
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import monocle.Focus

import scala.collection.immutable.SortedMap

private trait BrightnessesEditor[T]:
  def brightnesses: View[SortedMap[Band, BrightnessMeasure[T]]]
  def expanded: View[IsExpanded]
  def disabled: Boolean
