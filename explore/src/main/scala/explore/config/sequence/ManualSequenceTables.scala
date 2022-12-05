// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment

case class ManualSequenceTables(config: ManualConfig)
    extends ReactFnProps(ManualSequenceTables.component)

object ManualSequenceTables:
  private type Props = ManualSequenceTables

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { props =>
        <.div(^.height := "100%", ^.overflow.auto)(
          Segment(
            GmosSequenceTable.bracketDef,
            <.div(ExploreStyles.SequencesPanel)(
              Header("Acquisition"),
              GmosSequenceTable(props.config.acquisition),
              Header("Science"),
              GmosSequenceTable(props.config.science)
            )
          )
        )
      }
