// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.common.ConstraintSetObsQueriesGQL.AssignConstraintSetToObs
import explore.common.ConstraintsQueries._
import explore.common.ConstraintsQueriesGQL._
import explore.common.ObsQueries._
import explore.components.Tile
import explore.components.graphql.LiveQueryRenderMod
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.implicits._
import explore.model._
import explore.model.reusability._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.addons.select.Select
import react.semanticui.addons.select.Select.SelectItem
import react.semanticui.modules.dropdown.Dropdown

object ConstraintsTile {
  def constraintsTile(
    constraintsSetId: Option[ConstraintSet.Id],
    obsSummaryOpt:    Option[ObsSummary],
    constraintsInfo:  ConstraintsInfo
  )(implicit ctx:     AppContextIO) = {
    def constraintsSelectorFn(
      constraintsSetId: Option[ConstraintSet.Id],
      obsSummaryOpt:    Option[ObsSummary],
      observations:     ConstraintsInfo
    ): VdomNode =
      Select(
        value = constraintsSetId.map(_.show).orEmpty, // Set to empty string to clear
        placeholder = "Select a constraint set",
        onChange = (a: Dropdown.DropdownProps) =>
          (obsSummaryOpt, ConstraintSet.Id.parse(a.value.toString)).mapN { (obsId, csId) =>
            AssignConstraintSetToObs
              .execute(csId, obsId.id)
              .runAsyncAndForgetCB *> Callback.log(s"Set to $csId")
          }.getOrEmpty,
        options = observations.map(s => new SelectItem(value = s.id.show, text = s.name.value))
      )

    def renderConstraintsFn(
      csId:          ConstraintSet.Id,
      renderInTitle: Tile.RenderInTitle,
      csOpt:         View[Option[ConstraintSetModel]]
    ): VdomNode =
      csOpt.get.map { _ =>
        <.div(
          ExploreStyles.ConstraintsObsTile,
          ConstraintsPanel(csId, csOpt.zoom(_.get)(f => _.map(f)), renderInTitle)
        )
      }

    def renderConstraints(
      constraintsSetId: Option[ConstraintSet.Id],
      renderInTitle:    Tile.RenderInTitle
    ): VdomNode =
      constraintsSetId
        .map[VdomNode] { csId =>
          LiveQueryRenderMod[ObservationDB, ConstraintSetQuery.Data, Option[ConstraintSetModel]](
            ConstraintSetQuery.query(csId),
            _.constraintSet,
            List(ConstraintSetEditSubscription.subscribe[IO](csId))
          )((renderConstraintsFn _).reusable(csId, renderInTitle))
            .withKey(s"constraint-${constraintsSetId.foldMap(_.show)}")
        }
        .getOrElse(
          <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
                <.div("No constraints assigned")
          )
        )

    Tile(
      ObsTabTiles.ConstraintsId,
      "Constraints",
      canMinimize = true,
      control = ((constraintsSelectorFn _)
        .reusable(constraintsSetId, obsSummaryOpt, constraintsInfo))
        .some
    )(
      (renderConstraints _).reusable(constraintsSetId)
    )
  }

}
