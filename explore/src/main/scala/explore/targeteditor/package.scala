// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.option.*
import explore.model.TargetVisualOptions
import japgolly.scalajs.react.*
import lucuma.core.util.NewType
import lucuma.react.aladin.Fov
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document

extension (options: TargetVisualOptions) inline def fov: Fov = Fov(options.fovRA, options.fovDec)

extension (fov: Fov)
  def isDifferentEnough(newFov: Fov): Boolean =
    (fov.x.toMicroarcseconds - newFov.x.toMicroarcseconds).abs < 1e7 ||
      (fov.y.toMicroarcseconds - newFov.y.toMicroarcseconds).abs < 1e7

object AddDisabled extends NewType[Boolean]
type AddDisabled = AddDisabled.Type

def domRoot: Option[HTMLElement] =
  Option(document.querySelector(":root")) match
    case Some(r: HTMLElement) => r.some
    case _                    => none

def setVariable(root: Option[HTMLElement], variableName: String, value: Int): Callback =
  root
    .map(root =>
      Callback(root.style.setProperty(s"--aladin-image-$variableName", s"${value / 100.0}"))
    )
    .getOrEmpty
