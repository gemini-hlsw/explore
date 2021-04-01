// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.data.NonEmptyList
import cats.effect.IO
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.ConstraintSetModel
import explore.model.reusability._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.ui.reusability._
import react.common._

import ConstraintsQueries._

final case class ConstraintSetEditor(csid: ConstraintSet.Id)
    extends ReactProps[ConstraintSetEditor](ConstraintSetEditor.component)

object ConstraintSetEditor {
  type Props = ConstraintSetEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        AppCtx.runWithCtx { implicit appCtx =>
          LiveQueryRenderMod[ObservationDB, ConstraintSetQuery.Data, Option[ConstraintSetModel]](
            ConstraintSetQuery.query(props.csid),
            _.constraintSet,
            NonEmptyList.of(ConstraintSetEditSubscription.subscribe[IO](props.csid))
          ) { csOpt =>
            csOpt.get.map { _ =>
              ConstraintsPanel(props.csid, csOpt.zoom(_.get)(f => _.map(f)))
            }
          }
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
