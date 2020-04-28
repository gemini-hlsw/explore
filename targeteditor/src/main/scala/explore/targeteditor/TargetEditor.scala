// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.grid._
import react.semanticui.collections.form._
import react.semanticui.widths._
import react.aladin.Aladin
import react.common._
import gpp.ui.forms.FormInputEV
import crystal.react.implicits._
import explore._
import explore.model._
import react.semanticui.elements.icon.Icon
import japgolly.scalajs.react.extra.StateSnapshot
import monocle.macros.Lenses
import explore.components.ui.GPPStyles

final case class TargetEditor(
  target: ViewCtxIO[Option[ExploreTarget]]
) extends ReactProps {
  @inline override def render: VdomElement = TargetEditor.component(this)
  val searchTerm                           = target.zoomL(ExploreTarget.searchTermL).get
}

object TargetEditor {
  type Props = TargetEditor
  val AladinComp = Aladin.component

  trait AladinFacade {
    def hello: String
  }

  @Lenses
  final case class State(searchTerm: String)

  def updateSearchOp(p: Props)(
    value:              Option[String],
    cb:                 Callback = Callback.empty
  ): Callback =
    p.target
      .mod(_.map(_.copy(searchTerm = value.orEmpty)))
      .runInCB *> cb

  class Backend {
    // Create a mutable reference
    private val ref = Ref.toScalaComponent(AladinComp)

    def goTo(search: String): Callback =
      ref.get
        .flatMapCB(
          _.backend
            .gotoObject(search, (a, b) => Callback.log(s"córd: $a $b"), Callback.log("error"))
        )

    def render(p: Props) = {
      val searchEV =
        StateSnapshot[String](p.searchTerm)(updateSearchOp(p))
      <.div(
        ^.height := "100%",
        ^.width := "100%",
        Grid(columns = Two, padded = GridPadded.Horizontally)(
          ^.height := "100%",
          GridRow(
            GridColumn(stretched = true, computer = Four, clazz = GPPStyles.GPPForm)(
              Form(onSubmit          = goTo(searchEV.value))(
                FormInputEV(name     = "search",
                            id       = "search",
                            snapshot = searchEV,
                            label    = "Target",
                            focus    = true,
                            icon     = Icon("search"))
              )
            ),
            GridColumn(stretched = true, computer = Twelve)(
              AladinComp.withRef(ref)(
                Aladin(target = p.searchTerm, fov = 0.25, showGotoControl = false)
              )
            )
          )
        )
      )
    }
    // def onMount: Callback =
    // ref.foreachCB(
    // ref.get.flatMapCB(_.backend.gotoRaDec(0.0, 0.0))

  }

  val component =
    ScalaComponent
      .builder[Props]("TargetEditor")
      .renderBackend[Backend]
      // .componentDidMount(_.backend.onMount)
      // .componentWillReceiveProps(_.backend.onMount)
      // .initialStateFromProps { p => println("st"); State(p.searchTerm) }
      .build

}
