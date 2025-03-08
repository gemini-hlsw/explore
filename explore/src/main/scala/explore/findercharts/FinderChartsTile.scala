// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.attachments.*
import explore.attachments.ObsAttachmentUtils
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.FinderChartPreferences
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.Transformation
import explore.model.reusability.given
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.model.Program
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.ui.components.SolarProgress
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import monocle.Focus

import scala.collection.immutable.SortedSet

object FinderChartsTile:
  def apply(
    programId:        Program.Id,
    oid:              Observation.Id,
    attachmentIds:    View[SortedSet[Attachment.Id]],
    authToken:        Option[NonEmptyString],
    attachments:      View[AttachmentList],
    parallacticAngle: Option[Angle],
    readonly:         Boolean
  ) =
    Tile(
      ObsTabTileIds.FinderChartsId.id,
      s"Finder Charts",
      TileState(ChartSelector.Closed, None),
      bodyClass = ExploreStyles.FinderChartsTile
    )(
      tileState =>
        authToken
          .map[VdomNode]: t =>
            Body(
              programId,
              oid,
              t,
              attachmentIds,
              attachments,
              parallacticAngle,
              readonly,
              tileState
            )
          .orEmpty,
      (tileState, _) => Title(programId, authToken, attachmentIds, attachments, readonly)(tileState)
    )

  case class TileState(chartSelector: ChartSelector, selected: Option[Attachment.Id])

  object TileState:
    val chartSelector = Focus[TileState](_.chartSelector)
    val selected      = Focus[TileState](_.selected)

  private case class Body(
    programId:        Program.Id,
    oid:              Observation.Id,
    authToken:        NonEmptyString,
    attachmentIds:    View[SortedSet[Attachment.Id]],
    attachments:      View[AttachmentList],
    parallacticAngle: Option[Angle],
    readOnly:         Boolean,
    state:            View[TileState]
  ) extends ReactFnProps(Body.component) {
    val chartSelector = state.zoom(TileState.chartSelector)
    val selected      = state.zoom(TileState.selected)
  }

  private object Body extends ObsAttachmentUtils with FinderChartsAttachmentUtils:
    private type Props = Body

    private val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx       <- useContext(AppContext.ctx)
          client    <- useMemo(props.authToken): token =>
                         OdbRestClient[IO](ctx.environment, token)
          transform <- useStateView(Transformation.Default) // Current transformation
          urlMap    <- useStateView[UrlMap](Map.empty)
          // added attachment, FIXME once we can upload and assign in one step
          added     <- useState(none[Attachment.Id])
          action    <-                                      // If added associate with the observation
            useEffectWithDeps(added.value): _ =>
              // Associate the newly added attachment with the observation and select it
              added.value
                .map: newlyAdded =>
                  props.attachmentIds.mod(_ + newlyAdded) *>
                    transform.set(Transformation.Default) *>
                    props.selected.set(newlyAdded.some) *>
                    added.setState(none)
                .getOrEmpty
          _         <-
            useEffectWithDeps((props.authToken, props.attachments.get, props.attachmentIds.get)):
              (_, obsAttachments, obsAttachmentIds) =>
                val allCurrentKeys =
                  validAttachments(obsAttachments, obsAttachmentIds).values.map(_.toMapKey).toSet

                val newOas = allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

                val updateUrlMap =
                  urlMap.mod { umap =>
                    val filteredMap = umap.filter((k, _) => allCurrentKeys.contains(k))
                    newOas.foldRight(filteredMap)((key, m) => m.updated(key, pending))
                  }.toAsync

                val getUrls =
                  newOas.traverse_(key => getAttachmentUrl(client, key, urlMap))

                val defaultSelected =
                  if (allCurrentKeys.size === 1)
                    props.selected.set(allCurrentKeys.headOption.map(_._1))
                  else Callback.empty

                updateUrlMap *> getUrls *> defaultSelected.to[IO]
          _         <-                                      // Read preferences
            useEffectWithDeps((props.oid, props.selected.get)): (oid, aid) =>
              import ctx.given

              aid
                .map: aid =>
                  FinderChartPreferences
                    .queryWithDefault[IO](oid, aid)
                    .flatMap(transform.set(_).to[IO])
                    .runAsyncAndForget
                .getOrEmpty
          _         <-                                      // Write preferences
            useEffectWithDeps(transform.get): transform =>
              import ctx.given

              props.selected.get
                .map: aid =>
                  FinderChartPreferences
                    .updateTransformation[IO](props.oid, aid, transform)
                    .runAsyncAndForget
                .getOrEmpty
          action    <- useStateView(Action.None)
        yield
          val transforms = transform.get.calcTransform

          <.div(
            ExploreStyles.FinderChartsBackground,
            ^.onClick ==> { e =>
              props.chartSelector.set(ChartSelector.Closed).when_(props.chartSelector.get.value)
            }
          )(
            <.div(
              SolarProgress(ExploreStyles.FinderChartsLoadProgress)
                .unless(action.get === Action.None)
            ),
            ControlOverlay(props.parallacticAngle, transform),
            if (props.chartSelector.get.value)
              FinderChartLinker(
                props.programId,
                client,
                props.selected,
                props.attachmentIds,
                props.attachments.get
              )
            else EmptyVdom,
            <.div(ExploreStyles.FinderChartsBody)(
              props.selected.get.map { attId =>
                urlMap.get.find { case ((i, _), _) => i === attId }.map { url =>
                  url._2.renderPot(url =>
                    <.img(
                      ExploreStyles.FinderChartsImage,
                      ExploreStyles.FinderChartsImageInverted.when(transform.get.inverted.value),
                      ^.transform := transforms.mkString(" "),
                      ^.src       := url
                    )
                  )
                }
              }
            )
          )

  private case class Title(
    programId:     Program.Id,
    authToken:     Option[NonEmptyString],
    attachmentIds: View[SortedSet[Attachment.Id]],
    attachments:   View[AttachmentList],
    readonly:      Boolean
  )(val state: View[TileState])
      extends ReactFnProps(Title):
    val chartSelector = state.zoom(TileState.chartSelector)
    val selected      = state.zoom(TileState.selected)

  private object Title
      extends ReactFnComponent[Title](props =>
        for
          // added attachment, FIXME once we can upload and assign in one step
          added <- useState(none[Attachment.Id])
        yield attachmentSelector(
          props.programId,
          props.attachmentIds,
          props.attachments,
          props.authToken,
          props.selected,
          added,
          props.chartSelector,
          props.readonly
        )
      )
