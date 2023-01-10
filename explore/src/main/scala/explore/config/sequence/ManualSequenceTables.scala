// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.primereact.Panel

case class ManualSequenceTables(config: ManualConfig)
    extends ReactFnProps(ManualSequenceTables.component)

object ManualSequenceTables:
  private type Props = ManualSequenceTables

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { props =>
        <.div(^.height := "100%", ^.overflow.auto)(
          Panel()(
            <.div(ExploreStyles.SequencesPanel)(
              <.h3("Acquisition"),
              GmosSequenceTable(props.config.acquisition),
              <.h3("Science"),
              GmosSequenceTable(props.config.science)
            )
          )
        )
      }
