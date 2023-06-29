// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.attachments.AttachmentType
import explore.attachments.ObsAttachmentUtils
import explore.common.UserPreferencesQueries.FinderChartPreferences
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsAttachment
import explore.model.ObsAttachmentList
import explore.model.Transformation
import explore.model.reusability.given
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import lucuma.ui.components.SolarProgress
import lucuma.ui.primereact.FormDropdownOptional
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import react.common.ReactFnProps
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.primereact.SelectItem

import scala.collection.immutable.SortedSet

case class FinderCharts(
  programId:        Program.Id,
  oid:              Observation.Id,
  authToken:        NonEmptyString,
  obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
  obsAttachments:   View[ObsAttachmentList],
  renderInTitle:    Tile.RenderInTitle
) extends ReactFnProps(FinderCharts.component)

trait FinderChartsAttachmentUtils:
  def validAttachments(
    allAttachments:   ObsAttachmentList,
    obsAttachmentIds: SortedSet[ObsAtt.Id]
  ): ObsAttachmentList =
    allAttachments.filter { case (_, attachment) =>
      (attachment.attachmentType === ObsAttachmentType.Finder) && obsAttachmentIds
        .contains(attachment.id)
    }

object FinderCharts extends ObsAttachmentUtils with FinderChartsAttachmentUtils:
  private type Props = FinderCharts

  private def attachmentSelector(
    props:    Props,
    ctx:      AppContext[IO],
    client:   OdbRestClient[IO],
    selected: View[Option[ObsAtt.Id]],
    action:   View[Action],
    added:    UseState[Option[ObsAtt.Id]]
  ): VdomNode = {
    import ctx.given

    def addNewFinderChart(e: ReactEventFromInput) =
      action.set(Action.Insert) *>
        onInsertFileSelected(
          props.programId,
          props.obsAttachments,
          AttachmentType.Finder,
          client,
          action,
          id => added.setState(Some(id))
        )(e)

    <.div(
      ExploreStyles.FinderChartsSelectorSection,
      <.span(
        <.label(
          LabelButtonClasses,
          ^.htmlFor := "attachment-upload",
          Icons.FileArrowUp.withFixedWidth(true)
        ).withTooltip(
          tooltip = s"Upload new finder chart",
          placement = Placement.Right
        ),
        <.input(
          ExploreStyles.FileUpload,
          ^.tpe    := "file",
          ^.onChange ==> addNewFinderChart,
          ^.id     := "attachment-upload",
          ^.name   := "file",
          ^.accept := AttachmentType.Finder.accept
        )
      ),
      FormDropdownOptional(
        id = "attachment-selector".refined,
        placeholder = "Select finder chart",
        options = validAttachments(props.obsAttachments.get, props.obsAttachmentIds.get)
          .map(_._2)
          .map { attachment =>
            new SelectItem[ObsAttachment](value = attachment, label = attachment.fileName.value)
          }
          .toList,
        value = props.obsAttachments.get.find(i => selected.get.exists(_ === i._2.id)).map(_._2),
        onChange = (att: Option[ObsAttachment]) => selected.set(att.map(_.id))
      )
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken)((_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      )
      // Current transformation
      .useStateView(Transformation.Default)
      // selected attachment
      .useStateView(none[ObsAtt.Id])
      .useStateView[UrlMap](Map.empty)
      // added attachment, FIXME once we can upload and assign in one step
      .useState(
        none[ObsAtt.Id]
      )
      // If added associate with the observation
      .useEffectWithDepsBy((_, _, _, _, _, _, added) => added.value)(
        (props, _, _, transform, selected, _, added) =>
          _ =>
            // Associate the newly added attachment with the observation and select it
            added.value.map { newlyAdded =>
              props.obsAttachmentIds.mod(_ + newlyAdded) *> transform.set(
                Transformation.Default
              ) *> selected.set(newlyAdded.some) *> added
                .setState(none)
            }.getOrEmpty
      )
      .useEffectWithDepsBy((props, _, _, _, selected, _, _) =>
        (props.authToken, props.obsAttachments.get, props.obsAttachmentIds.get)
      )((props, _, client, _, selected, urlMap, _) =>
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
            if (allCurrentKeys.size === 1) selected.set(allCurrentKeys.headOption.map(_._1))
            else Callback.empty

          updateUrlMap *> getUrls *> defaultSelected.to[IO]
      )
      // Read preferences
      .useEffectWithDepsBy((props, _, _, _, selected, _, _) => (props.oid, selected.get)) {
        (props, ctx, _, transform, _, _, _) => (oid, aid) =>
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
      .useEffectWithDepsBy((props, _, _, transform, _, _, _) => transform.get) {
        (props, ctx, _, transform, selected, _, _) => transform =>
          import ctx.given

          selected.get
            .map(aid =>
              FinderChartPreferences
                .updateTransformation[IO](props.oid, aid, transform)
                .runAsyncAndForget
            )
            .getOrEmpty
      }
      .useStateView(Action.None)
      .render { (props, ctx, client, ops, selectedAttachment, urls, added, action) =>
        val transforms = ops.get.calcTransform
        ReactFragment(
          props.renderInTitle(
            attachmentSelector(props, ctx, client, selectedAttachment, action, added)
          ),
          <.div(
            SolarProgress(ExploreStyles.FinderChartsLoadProgress)
              .unless(action.get === Action.None)
          ),
          ControlOverlay(ops),
          <.div(
            ExploreStyles.FinderChartsBody,
            selectedAttachment.get.map { attId =>
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
