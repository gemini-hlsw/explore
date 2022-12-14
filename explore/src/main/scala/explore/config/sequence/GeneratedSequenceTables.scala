// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment

case class GeneratedSequenceTables(obsId: Observation.Id, config: FutureExecutionConfig)
    extends ReactFnProps(GeneratedSequenceTables.component)

object GeneratedSequenceTables:
  private type Props = GeneratedSequenceTables

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { props =>
        <.div(^.height := "100%", ^.overflow.auto)(
          Segment(
            GmosSequenceTable.bracketDef,
            <.div(ExploreStyles.SequencesPanel)(
              VisitsViewer(props.obsId),
              Header("Acquisition"),
              GmosSequenceTable(
                props.config.acquisition.nextAtom +: props.config.acquisition.possibleFuture
              ),
              Header("Science"),
              GmosSequenceTable(
                props.config.science.nextAtom +: props.config.science.possibleFuture
              )
            )
          )
        )
      }
