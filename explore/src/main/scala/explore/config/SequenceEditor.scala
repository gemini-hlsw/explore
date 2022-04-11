// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRender
import explore.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability._
import queries.common.SequenceStepsGQL._
import react.common._

final case class SequenceEditor(programId: Program.Id)
    extends ReactFnProps[SequenceEditor](SequenceEditor.component)

object SequenceEditor {
  type Props = SequenceEditor

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  private def renderFn(config: Option[SequenceSteps.Data.Observations.Nodes.Config]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(SequenceTable.apply)

  val component =
    ScalaFnComponent
      .withReuse[Props](props =>
        AppCtx.using { implicit ctx =>
          LiveQueryRender[ObservationDB,
                          SequenceSteps.Data,
                          Option[SequenceSteps.Data.Observations.Nodes.Config]
          ](
            SequenceSteps.query(props.programId).reuseAlways,
            ((_: SequenceSteps.Data).observations.nodes.headOption.flatMap(_.config)).reuseAlways,
            List.empty.reuseAlways
          )(
            potRender((renderFn _).reuseAlways)
          )
        }
      )
}
