// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Resources
import explore.attachments.ObsAttachmentUtils
import explore.common.UserPreferencesQueries.FinderChartPreferences
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsAttachmentList
import explore.model.Transformation
import explore.model.reusability.given
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import monocle.Focus
import monocle.Lens
import monocle.Prism
import react.common.ReactFnProps

import scala.collection.immutable.SortedSet

case class FinderCharts(
  programId:        Program.Id,
  oid:              Observation.Id,
  authToken:        NonEmptyString,
  obsAttachmentIds: View[SortedSet[ObsAttachment.Id]],
  obsAttachments:   View[ObsAttachmentList]
) extends ReactFnProps(FinderCharts.component)

trait FinderChartsAttachmentUtils:
  def validAttachments(
    allAttachments:   ObsAttachmentList,
    obsAttachmentIds: SortedSet[ObsAttachment.Id]
  ): ObsAttachmentList =
    allAttachments.filter { case (_, attachment) =>
      (attachment.attachmentType === ObsAttachmentType.Finder) && obsAttachmentIds
        .contains(attachment.id)
    }

object FinderCharts extends ObsAttachmentUtils with FinderChartsAttachmentUtils:
  private type Props = FinderCharts

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken)((_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      )
      .useStateView(Transformation.Default)
      .useStateView(none[ObsAttachment.Id])
      .useStateView[UrlMap](Map.empty)
      .useEffectWithDepsBy((props, _, _, _, selected, _) =>
        (selected.get, props.obsAttachments.get, props.obsAttachmentIds.get)
      )((props, _, client, _, _, urlMap) =>
        (sel, obsAttachments, obsAttachmentIds) =>
          val allCurrentKeys =
            validAttachments(obsAttachments, obsAttachmentIds).values.map(_.toMapKey).toSet
          val newOas         = allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

          val updateUrlMap =
            urlMap.mod { umap =>
              val filteredMap = umap.filter((k, v) => allCurrentKeys.contains(k))
              newOas.foldRight(filteredMap)((key, m) => m.updated(key, Pot.pending))
            }.toAsync

          val getUrls =
            newOas.traverse_(key => getAttachmentUrl(props.programId, client, key, urlMap))

          updateUrlMap *> getUrls
      )
      // Read preferences
      .useEffectWithDepsBy((props, _, _, _, selected, _) => (props.oid, selected.get)) {
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
        (props, ctx, _, transform, selected, _) => transform =>
          import ctx.given

          selected.get
            .map(aid =>
              FinderChartPreferences
                .updateTransformation[IO](props.oid, aid, transform)
                .runAsyncAndForget
            )
            .getOrEmpty
      }
      .render { (props, _, client, ops, selectedAttachment, urls) =>
        val transforms = ops.get.calcTransform
        ReactFragment(
          ControlOverlay(ops),
          AttachmentsOverlay(props.programId,
                             client,
                             selectedAttachment,
                             props.obsAttachmentIds,
                             props.obsAttachments
          ),
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
