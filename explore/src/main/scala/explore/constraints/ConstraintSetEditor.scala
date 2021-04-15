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
import explore.utils._

final case class ConstraintSetEditor(csid: ConstraintSet.Id)
    extends ReactProps[ConstraintSetEditor](ConstraintSetEditor.component)

object ConstraintSetEditor {
  type Props = ConstraintSetEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected def renderFn(
    csid:  ConstraintSet.Id,
    csOpt: View[Option[ConstraintSetModel]]
  ): VdomNode =
    csOpt.get.map { _ =>
      ConstraintsPanel(csid, csOpt.zoom(_.get)(f => _.map(f)))
    }

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        AppCtx.using { implicit appCtx =>
          LiveQueryRenderMod[ObservationDB, ConstraintSetQuery.Data, Option[ConstraintSetModel]](
            ConstraintSetQuery.query(props.csid),
            _.constraintSet,
            NonEmptyList.of(ConstraintSetEditSubscription.subscribe[IO](props.csid))
          )((renderFn _).reuse(props.csid))
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
