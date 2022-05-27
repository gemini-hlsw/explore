// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.hooks._
import explore.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.sequence._
import lucuma.ui.reusability._
import queries.common.GeneratedSequenceSQL._
import react.common._

final case class GeneratedSequenceViewer(obsId: Observation.Id)(implicit val ctx: AppContextIO)
    extends ReactFnProps[GeneratedSequenceViewer](GeneratedSequenceViewer.component)

object GeneratedSequenceViewer {
  type Props = GeneratedSequenceViewer

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  private def renderFn(config: Option[FutureExecutionConfig]): VdomNode =
    config.fold[VdomNode](<.div("Default observation not found"))(GeneratedSequenceTables.apply)

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useEffectResultOnMountBy { props =>
        implicit val ctx = props.ctx

        SequenceSteps
          .query(props.obsId)
          .map(_.observation.flatMap(_.execution.config))
      }
      .render((_, config) => potRender(renderFn)(config))
}
