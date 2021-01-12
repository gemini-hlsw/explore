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
import explore.model.enum.AppTab
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.utils._
import react.common.ReactProps
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

final case class ObsList(
  observations: View[List[ObsSummary]],
  focused:      View[Option[Focused]]
) extends ReactProps[ObsList](ObsList.component)

object ObsList {
  type Props = ObsList

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[Props]
      .render_P { (props: Props) =>
        AppCtx.withCtx { ctx =>
          <.div(ExploreStyles.ObsTreeWrapper)(
            <.div(ExploreStyles.TreeToolbar)(
              <.div(
                Button(size = Mini, compact = true, disabled = true)(
                  Icons.New.size(Small).fitted(true)
                )
              )
            ),
            <.div(ExploreStyles.ObsTree)(
              <.div(ExploreStyles.ObsScrollTree)(
                props.observations.get.toTagMod { obs =>
                  val focusedObs = FocusedObs(obs.id)
                  val selected   = props.focused.get.exists(_ === focusedObs)
                  <.a(
                    ^.href := ctx.pageUrl(AppTab.Observations, focusedObs.some),
                    ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                    ^.onClick ==> linkOverride(props.focused.set(focusedObs.some))
                  )(
                    ObsBadge(obs, ObsBadge.Layout.NameAndConf, selected = selected)
                  )
                }
              )
            )
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
