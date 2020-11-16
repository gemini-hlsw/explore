// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.ObsBadge
import explore.components.ui.ExploreStyles
import explore.model.Focused
import explore.model.Focused.FocusedObs
import explore.model.ObsSummary
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.common.implicits._

final case class ObsList(
  observations: View[List[ObsSummary]],
  focused:      View[Option[Focused]]
) extends ReactProps[ObsList](ObsList.component)

object ObsList {
  type Props = ObsList

  class Backend() {
    def render(props: Props): VdomElement =
      <.div(ExploreStyles.ObsTree)(
        <.div(
          props.observations.get.toTagMod { obs =>
            val selected = props.focused.get.exists(_ === FocusedObs(obs.id))
            <.div(
              ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
              ^.cursor.pointer,
              ^.onClick ==> { e: ReactEvent =>
                e.stopPropagationCB >>
                  props.focused
                    .set(FocusedObs(obs.id).some)
                    .runAsyncCB
              }
            )(
              ObsBadge(obs, ObsBadge.Layout.NameAndConf, selected = selected)
            )
          }
        )
      )

  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .backend(_ => new Backend())
      .renderBackend
      .build
}
