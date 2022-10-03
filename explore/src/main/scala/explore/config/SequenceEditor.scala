// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.hooks.*
import explore.model.AppContext
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.ui.syntax.all.given
import queries.common.ManualSequenceGQL.*
import queries.schemas.implicits.*
import react.common.ReactFnProps

case class SequenceEditor(programId: Program.Id) extends ReactFnProps(SequenceEditor.component)

object SequenceEditor:
  private type Props = SequenceEditor

  private def renderFn(config: Option[SequenceSteps.Data.Observations.Matches.Config]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(ManualSequenceTables.apply)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useEffectResultOnMountBy { (props, ctx) =>
        import ctx.given

        SequenceSteps
          .query(props.programId.toWhereObservation)
          .map(_.observations.matches.headOption.flatMap(_.config))
      }
      .render((_, _, config) => potRender(renderFn)(config))
