// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.AppCtx
import explore.common.ConstraintsQueries._
import explore.common.ConstraintsQueriesGQL._
import explore.components.Tile
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.Focused
import explore.model.reusability._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.ui.reusability._
import lucuma.ui.reuse._
import react.common._

final case class ConstraintSetEditor(
  csId:          ConstraintSet.Id,
  focused:       View[Option[Focused]],
  renderInTitle: Tile.RenderInTitle
) extends ReactProps[ConstraintSetEditor](ConstraintSetEditor.component)

object ConstraintSetEditor {
  type Props = ConstraintSetEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        AppCtx.using { implicit appCtx =>
          LiveQueryRenderMod[ObservationDB, ConstraintSetQuery.Data, Option[ConstraintSetModel]](
            ConstraintSetQuery.query(props.csId),
            _.constraintSet,
            List(ConstraintSetEditSubscription.subscribe[IO](props.csId))
          )(
            Reuse
              .by(props)((csOpt: View[Option[ConstraintSetModel]]) =>
                csOpt.get.map { _ =>
                  ConstraintsPanel(
                    props.csId,
                    csOpt.zoom(_.get)(f => _.map(f)),
                    props.renderInTitle,
                    allowMultiEdit = true,
                    onCopy = (
                      (id: ConstraintSet.Id) =>
                        props.focused.set(Focused.FocusedConstraintSet(id).some)
                    ).reuseAlways
                  )
                }
              )
          )
        }

      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
