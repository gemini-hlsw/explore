package explore.targeteditor

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import react.common._
import crystal.react.View
import lucuma.core.model.SourceProfile

import explore.model.reusability._
import crystal.react.implicits._

case class SourceProfileEditor(sourceProfile: View[SourceProfile], disabled: Boolean)
    extends ReactFnProps[SourceProfileEditor](SourceProfileEditor.component)

object SourceProfileEditor {
  type Props = SourceProfileEditor

  protected implicit val reuseProps: Reusability[Props] = Reusability.derive

  val component = ScalaFnComponent.withReuse[Props](props =>
    <.div(
      props.sourceProfile
        .zoom(SourceProfile.integratedBrightnesses)
        .mapValue(integratedBrightnessesView =>
          IntegratedBrightnessEditor(integratedBrightnessesView, props.disabled)
        // <.div(integratedBrightnessesView.get.toString())
        )
        .whenDefined
    )
  )
}
