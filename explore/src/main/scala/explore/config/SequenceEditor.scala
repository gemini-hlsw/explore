// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.hooks._
import explore.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.ui.syntax.all.given
import queries.common.ManualSequenceGQL._
import queries.schemas.implicits._
import react.common.ReactFnProps

final case class SequenceEditor(programId: Program.Id)(implicit val ctx: AppContextIO)
    extends ReactFnProps[SequenceEditor](SequenceEditor.component)

object SequenceEditor {
  type Props = SequenceEditor

  private def renderFn(config: Option[SequenceSteps.Data.Observations.Matches.Config]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(ManualSequenceTables.apply)

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useEffectResultOnMountBy { props =>
        implicit val ctx = props.ctx

        SequenceSteps
          .query(props.programId.toWhereObservation)
          .map(_.observations.matches.headOption.flatMap(_.config))
      }
      .render((_, config) => potRender(renderFn)(config))
}
