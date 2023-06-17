// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import crystal.react.hooks.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.primereact.Divider
import explore.model.ObsAttachmentList
import explore.attachments.AttachmentType
import react.floatingui.Placement
import react.floatingui.syntax.*
import lucuma.core.model.Program
import explore.attachments.ObsAttachmentUtils
import crystal.react.View
import explore.model.AppContext
import cats.effect.IO
import explore.utils.OdbRestClient
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ui.reusability.given
import scala.collection.immutable.SortedSet
import lucuma.core.model.ObsAttachment
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType

case class AttachmentsOverlay(
  programId:        Program.Id,
  authToken:        NonEmptyString,
  obsAttachmentIds: View[SortedSet[ObsAttachment.Id]],
  obsAttachments:   View[ObsAttachmentList]
) extends ReactFnProps[AttachmentsOverlay](AttachmentsOverlay.component)

object AttachmentsOverlay extends ObsAttachmentUtils {
  type Props = AttachmentsOverlay

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken)((_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      )
      .useStateView(Action.None)
      .render { (p, ctx, odbRestClient, action) =>
        import ctx.given

        def addNewFinderChart(e: ReactEventFromInput) =
          onInsertFileSelected(p.programId,
                               p.obsAttachments,
                               AttachmentType.Finder,
                               odbRestClient,
                               action,
                               id => p.obsAttachmentIds.mod(_ + id)
          )(e)
        val obsAttachments                            = p.obsAttachments.get.filter { case (_, attachment) =>
          (attachment.attachmentType === ObsAttachmentType.Finder) && p.obsAttachmentIds.get
            .contains(attachment.id)
        }
        pprint.pprintln(obsAttachments)
        pprint.pprintln(p.obsAttachmentIds.get)
        ReactFragment(
          <.div(
            ExploreStyles.FinderChartsAttachments,
            <.span(
              Icons.Files,
              "Attachments",
              <.label(
                // labelButtonClasses,
                ^.htmlFor := "attachment-upload",
                Icons.FileArrowUp
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
            Divider()
          )
        )
      }
}
