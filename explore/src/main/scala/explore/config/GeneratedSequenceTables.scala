// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.sequence._
import lucuma.ui.reusability._
import react.common._
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment

final case class GeneratedSequenceTables(config: FutureExecutionConfig)
    extends ReactFnProps[GeneratedSequenceTables](GeneratedSequenceTables.component)

object GeneratedSequenceTables {
  type Props = GeneratedSequenceTables

  implicit private val propsReuse: Reusability[Props] = Reusability.derive

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .renderWithReuse { props =>
        <.div(^.height := "100%", ^.overflow.auto)(
          Segment()(
            SequenceTable.bracketDef,
            <.div(
              Header("Acquisition"),
              SequenceTable(
                props.config.acquisition.nextAtom +: props.config.acquisition.possibleFuture
              ),
              Header("Science"),
              SequenceTable(props.config.science.nextAtom +: props.config.science.possibleFuture)
            )
          )
        )
      }
}
