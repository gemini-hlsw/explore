// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import scala.scalajs.js.annotation.JSExportTopLevel
import japgolly.scalajs.react.vdom.html_<^._
import explore.AppMain
import explore._
import explore.model.RootModel
import cats.effect.concurrent.Ref
import cats.effect.IO
import explore.model.ExploreObservation
import crystal.ViewF
import fs2.concurrent.SignallingRef
import cats.effect.SyncIO
import crystal.react.StreamRendererMod
import explore.model.reusability._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.extra.router.RouterLogic
import explore.model.Page
import japgolly.scalajs.react.Callback

@JSExportTopLevel("ObsTreeTest")
object Test extends AppMain {

  val obsRef = SignallingRef
    .in[SyncIO, IO, List[ExploreObservation]](TargetTreeTest.observations)
    .unsafeRunSync()

  val Component = StreamRendererMod.build(obsRef.discrete)

  override def rootComponent(view: View[RootModel]): VdomElement =
    // AndOrTest.render
    // TargetTree(TargetTreeTest.targets, TargetTreeTest.observations)
    // TargetObsList(TargetTreeTest.targets, ViewF(obs.get.unsafeRunSync(), obs.update))
    Component(
      _.map[VdomNode](obsView =>
        TargetObsList(TargetTreeTest.targets,
                      obsView,
                      view.zoomO(RootModel.focusedTargetOrObsId),
                      _ => Callback.empty
        )
      ).toOption
        .getOrElse(<.div)
    )
}
