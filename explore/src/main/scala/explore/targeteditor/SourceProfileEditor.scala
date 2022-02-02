// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.View
import crystal.react.implicits._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SourceProfile
import react.common._
import explore.components.ui.ExploreStyles

case class SourceProfileEditor(sourceProfile: View[SourceProfile], disabled: Boolean)
    extends ReactFnProps[SourceProfileEditor](SourceProfileEditor.component)

object SourceProfileEditor {
  type Props = SourceProfileEditor

  protected implicit val reuseProps: Reusability[Props] = Reusability.derive

  val component = ScalaFnComponent.withReuse[Props](props =>
    <.div(
      ExploreStyles.BrightnessCell,
      props.sourceProfile
        .zoom(SourceProfile.integratedBrightnesses)
        .mapValue(integratedBrightnessesView =>
          IntegratedBrightnessEditor(integratedBrightnessesView, props.disabled)
        )
        .whenDefined
    )
  )
}
