// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.ObsBadge
import explore.components.ui.GPPStyles
import explore.model.Focused
import explore.model.Focused.FocusedObs
import explore.model.ObsSummary
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps

final case class ObsList(
  observations: View[List[ObsSummary]],
  focused:      View[Option[Focused]]
) extends ReactProps[ObsList](ObsList.component)

object ObsList {
  type Props = ObsList

  class Backend() {
    def render(props: Props): VdomElement = {
      println(props)
      <.div(GPPStyles.ObsTree)(
        <.div(
          props.observations.get.toTagMod { obs =>
            <.div(
              GPPStyles.ObsItem,
              ^.cursor.pointer,
              ^.onClick ==> { e: ReactEvent =>
                e.stopPropagationCB >>
                  props.focused
                    .set(FocusedObs(obs.id).some)
                    .runInCB
              }
            )(
              ObsBadge(obs,
                       ObsBadge.Layout.NameAndConf,
                       selected = props.focused.get
                         .exists(_ === FocusedObs(obs.id))
              )
            )
          }
        )
      )
    }

  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .backend(_ => new Backend())
      .renderBackend
      .build
}
