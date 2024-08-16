// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.attachments.Action
import explore.attachments.ObsAttachmentUtils
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.FinderChartPreferences
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsAttachment
import explore.model.ObsAttachmentList
import explore.model.Observation
import explore.model.Transformation
import explore.model.reusability.given
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.model.ObsAttachment as ObsAtt
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.ui.components.SolarProgress
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import monocle.Focus

import scala.collection.immutable.SortedSet

case class FinderChartsTileState(chartSelector: ChartSelector, selected: Option[ObsAtt.Id])

object FinderChartsTileState:
  val chartSelector = Focus[FinderChartsTileState](_.chartSelector)
  val selected      = Focus[FinderChartsTileState](_.selected)

case class FinderChartsBody(
  programId:        Program.Id,
  oid:              Observation.Id,
  authToken:        NonEmptyString,
  obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
  obsAttachments:   View[ObsAttachmentList],
  parallacticAngle: Option[Angle],
  readOnly:         Boolean,
  state:            View[FinderChartsTileState]
) extends ReactFnProps(FinderChartsBody.component) {
  val chartSelector = state.zoom(FinderChartsTileState.chartSelector)
  val selected      = state.zoom(FinderChartsTileState.selected)
}

object FinderChartsBody extends ObsAttachmentUtils with FinderChartsAttachmentUtils:
  private type Props = FinderChartsBody

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken)((_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      )
      // Current transformation
      .useStateView(Transformation.Default)
      .useStateView[UrlMap](Map.empty)
      // added attachment, FIXME once we can upload and assign in one step
      .useState(
        none[ObsAtt.Id]
      )
      // If added associate with the observation
      .useEffectWithDepsBy((_, _, _, _, _, added) => added.value)(
        (props, _, _, transform, _, added) =>
          _ =>
            // Associate the newly added attachment with the observation and select it
            added.value.map { newlyAdded =>
              props.obsAttachmentIds.mod(_ + newlyAdded) *> transform.set(
                Transformation.Default
              ) *> props.selected.set(newlyAdded.some) *> added
                .setState(none)
            }.getOrEmpty
      )
      .useEffectWithDepsBy((props, _, _, _, _, _) =>
        (props.authToken, props.obsAttachments.get, props.obsAttachmentIds.get)
      )((props, _, client, _, urlMap, _) =>
        (_, obsAttachments, obsAttachmentIds) =>
          val allCurrentKeys =
            validAttachments(obsAttachments, obsAttachmentIds).values.map(_.toMapKey).toSet

          val newOas = allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

          val updateUrlMap =
            urlMap.mod { umap =>
              val filteredMap = umap.filter((k, _) => allCurrentKeys.contains(k))
              newOas.foldRight(filteredMap)((key, m) => m.updated(key, Pot.pending))
            }.toAsync

          val getUrls =
            newOas.traverse_(key => getAttachmentUrl(props.programId, client, key, urlMap))

          val defaultSelected =
            if (allCurrentKeys.size === 1) props.selected.set(allCurrentKeys.headOption.map(_._1))
            else Callback.empty

          updateUrlMap *> getUrls *> defaultSelected.to[IO]
      )
      // Read preferences
      .useEffectWithDepsBy((props, _, _, _, _, _) => (props.oid, props.selected.get)) {
        (props, ctx, _, transform, _, _) => (oid, aid) =>
          import ctx.given

          aid
            .map(aid =>
              FinderChartPreferences
                .queryWithDefault[IO](oid, aid)
                .flatMap { t =>
                  transform.set(t).to[IO]
                }
                .runAsyncAndForget
            )
            .getOrEmpty
      }
      // Write preferences
      .useEffectWithDepsBy((props, _, _, transform, _, _) => transform.get) {
        (props, ctx, _, transform, _, _) => transform =>
          import ctx.given

          props.selected.get
            .map(aid =>
              FinderChartPreferences
                .updateTransformation[IO](props.oid, aid, transform)
                .runAsyncAndForget
            )
            .getOrEmpty
      }
      .useStateView(Action.None)
      .render { (props, ctx, client, ops, urls, added, action) =>
        val transforms = ops.get.calcTransform
        <.div(
          ExploreStyles.FinderChartsBackground,
          ^.onClick ==> { e =>
            props.chartSelector.set(ChartSelector.Closed).when_(props.chartSelector.get.value)
          },
          <.div(
            SolarProgress(ExploreStyles.FinderChartsLoadProgress)
              .unless(action.get === Action.None)
          ),
          ControlOverlay(props.parallacticAngle, ops),
          if (props.chartSelector.get.value) {
            FinderChartLinker(props.programId,
                              client,
                              props.selected,
                              props.obsAttachmentIds,
                              props.obsAttachments.get
            )
          } else EmptyVdom,
          <.div(
            ExploreStyles.FinderChartsBody,
            props.selected.get.map { attId =>
              urls.get.find { case ((i, _), _) => i === attId }.map { url =>
                url._2.renderPot(url =>
                  <.img(
                    ExploreStyles.FinderChartsImage,
                    ExploreStyles.FinderChartsImageInverted.when(ops.get.inverted.value),
                    ^.transform := transforms.mkString(" "),
                    ^.src       := url
                  )
                )
              }
            }
          )
        )
      }

case class FinderChartsTitle(
  programId:        Program.Id,
  authToken:        NonEmptyString,
  obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
  obsAttachments:   View[ObsAttachmentList],
  readOnly:         Boolean
)(val state: View[FinderChartsTileState])
    extends ReactFnProps(FinderChartsTitle.component):
  val chartSelector = state.zoom(FinderChartsTileState.chartSelector)
  val selected      = state.zoom(FinderChartsTileState.selected)

object FinderChartsTitle:
  private type Props = FinderChartsTitle

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken): (_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      .useStateView(Action.None)
      // added attachment, FIXME once we can upload and assign in one step
      .useState(none[ObsAtt.Id])
      .render: (props, ctx, restClient, action, added) =>
        attachmentSelector(props.programId,
                           props.obsAttachmentIds,
                           props.obsAttachments,
                           ctx,
                           restClient,
                           props.selected,
                           action,
                           added,
                           props.chartSelector,
                           props.readOnly
        )
