// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ConstraintSetObsQueries._
import explore.common.ConstraintSetObsQueriesGQL
import explore.common.ConstraintSetObsQueriesGQL._
import explore.common.ConstraintsQueries._
import explore.common.ConstraintsQueriesGQL._
import explore.common.ObsQueries._
import explore.components.Tile
import explore.components.graphql.LiveQueryRenderMod
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.implicits._
import explore.model._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.addons.select.Select
import react.semanticui.addons.select.Select.SelectItem
import react.semanticui.elements.button.Button
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.sizes

import scala.util.Random

object ConstraintsTile {

  private def newConstraintSet(
    obsId:           Observation.Id
  )(implicit client: TransactionalClient[IO, ObservationDB]): IO[Unit] =
    for {
      id <-
        IO(Random.nextInt(0xfff)).map(int =>
          ConstraintSet.Id(PosLong.unsafeFrom(int.abs.toLong + 1))
        )
      _  <-
        AddConstraintSet
          .execute(
            defaultCreateConstraintSet(
              defaultConstraintSetResult(id, NonEmptyString.unsafeFrom(s"New Constraints [$id]"))
            )
          )
          .void
      _  <- AssignConstraintSetToObs.execute(id, obsId)
    } yield ()

  def constraintsTile(
    constraintSetId: Option[ConstraintSet.Id],
    obsSummaryOpt:   Option[ObsSummary],
    constraintsInfo: ConstraintsInfo
  )(implicit ctx:    AppContextIO): Tile = {
    def constraintsSelectorFn(
      constraintSetId: Option[ConstraintSet.Id],
      obsId:           Observation.Id,
      observations:    ConstraintsInfo
    ): VdomNode =
      <.span(
        Select(
          value = constraintSetId.map(_.show).orEmpty, // Set to empty string to clear
          placeholder = "Select a constraint set",
          className = "small",                         // Not exposed in React component properties but works in SUI.
          onChange = (a: Dropdown.DropdownProps) =>
            ConstraintSet.Id
              .parse(a.value.toString)
              .map(csId =>
                AssignConstraintSetToObs
                  .execute(csId, obsId)
                  .runAsyncAndForgetCB *> Callback.log(s"Set to $csId")
              )
              .getOrEmpty,
          options = observations.map(s => new SelectItem(value = s.id.show, text = s.name.value))
        ),
        Button(
          size = sizes.Tiny,
          compact = true,
          clazz = ExploreStyles.TitleButtonNew |+| ExploreStyles.VeryCompact,
          icon = Icons.New,
          content = "New",
          onClick = newConstraintSet(obsId).runAsyncCB
        )
      )

    def onCopy(newId: ConstraintSet.Id): IO[Unit] =
      obsSummaryOpt
        .map(obs => ConstraintSetObsQueriesGQL.AssignConstraintSetToObs.execute(newId, obs.id).void)
        .orEmpty

    def renderConstraintsFn(
      csId:          ConstraintSet.Id,
      renderInTitle: Tile.RenderInTitle,
      csOpt:         View[Option[ConstraintSetModel]]
    ): VdomNode =
      csOpt.get.map { _ =>
        <.div(
          ExploreStyles.ConstraintsObsTile,
          ConstraintsPanel(csId,
                           csOpt.zoom(_.get)(f => _.map(f)),
                           renderInTitle,
                           allowMultiEdit = false,
                           (onCopy _).reusable
          )
        )
      }

    def renderConstraints(
      constraintSetId: Option[ConstraintSet.Id],
      renderInTitle:   Tile.RenderInTitle
    ): VdomNode =
      constraintSetId
        .map[VdomNode] { csId =>
          LiveQueryRenderMod[ObservationDB, ConstraintSetQuery.Data, Option[ConstraintSetModel]](
            ConstraintSetQuery.query(csId),
            _.constraintSet,
            List(ConstraintSetEditSubscription.subscribe[IO](csId))
          )((renderConstraintsFn _).reusable(csId, renderInTitle))
            .withKey(s"constraint-${constraintSetId.foldMap(_.show)}")
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
      control = obsSummaryOpt.map(obsSummary =>
        ((constraintsSelectorFn _).reusable(constraintSetId, obsSummary.id, constraintsInfo))
      )
    )(
      (renderConstraints _).reusable(constraintSetId)
    )
  }

}
