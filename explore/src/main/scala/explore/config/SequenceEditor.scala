// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.reuse._
import explore.AppCtx
import explore.common.SequenceStepsGQL._
import explore.components.graphql.LiveQueryRender
import explore.implicits._
import explore.schemas.ObservationDB
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._

final case class SequenceEditor() extends ReactProps[SequenceEditor](SequenceEditor.component)

object SequenceEditor {
  type Props = SequenceEditor

  private def renderFn(config: Option[SequenceSteps.Data.Observations.Nodes.Config]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(SequenceTable.apply)

  val component =
    ScalaComponent
      .builder[Props]
      .render(_ =>
        AppCtx.using { implicit ctx =>
          LiveQueryRender[ObservationDB,
                          SequenceSteps.Data,
                          Option[SequenceSteps.Data.Observations.Nodes.Config]
          ](SequenceSteps.query().reuseAlways,
            ((_: SequenceSteps.Data).observations.nodes.headOption.flatMap(_.config)).reuseAlways,
            List.empty.reuseAlways
          )(
            potRender((renderFn _).reuseAlways)
          )
        }
      )
      .build
}
