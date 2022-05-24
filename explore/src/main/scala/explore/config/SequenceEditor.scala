// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.effect.Resource
import crystal.react.StreamResourceRenderer
import crystal.react.reuse._
import explore.AppCtx
import explore.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.ui.reusability._
import queries.common.ManualSequenceGQL._
import react.common._

final case class SequenceEditor(programId: Program.Id)
    extends ReactFnProps[SequenceEditor](SequenceEditor.component)

object SequenceEditor {
  type Props = SequenceEditor

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  private def renderFn(config: Option[SequenceSteps.Data.Observations.Nodes.Config]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(ManualSequenceTables.apply)

  val component =
    ScalaFnComponent
      .withReuse[Props](props =>
        AppCtx.using { implicit ctx =>
          StreamResourceRenderer[Option[SequenceSteps.Data.Observations.Nodes.Config]](
            Resource.eval(
              IO(
                fs2.Stream.eval(
                  SequenceSteps
                    .query(props.programId)
                    .map(_.observations.nodes.headOption.flatMap(_.config))
                )
              )
            ),
            potRenderWithReuse((renderFn _).reuseAlways)
          )
        }
      )
}
