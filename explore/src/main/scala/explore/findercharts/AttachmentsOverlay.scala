// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.Order.*
import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import crystal.react.hooks.*
import crystal.react.implicits.*
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
import explore.model.ObsAttachment
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import react.primereact.PrimeStyles
import lucuma.ui.primereact.LucumaStyles
import lucuma.react.table.*
import crystal.react.reuse.*
import lucuma.ui.table.*
// import explore.utils.*
// import explore.syntax.ui.*
// import lucuma.react.syntax.*
// import lucuma.ui.table.*
import lucuma.ui.utils.*

case class AttachmentsOverlay(
  programId:        Program.Id,
  authToken:        NonEmptyString,
  obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
  obsAttachments:   View[ObsAttachmentList]
) extends ReactFnProps[AttachmentsOverlay](AttachmentsOverlay.component)

object AttachmentsOverlay extends ObsAttachmentUtils {
  private type Props = AttachmentsOverlay

  private val columnNames: Map[ColumnId, String] = Map(
    ActionsColumnId  -> "Actions",
    FileNameColumnId -> "File"
  )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken)((_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      )
      .useStateView(Action.None)
      .useMemoBy((props, _, _, _) => ())((_, _, _, _) =>
        _ =>
          // import ctx.given

          def column[V](id: ColumnId, accessor: ObsAttachment => V)
            : ColumnDef.Single[View[ObsAttachment], V] =
            ColDef(id, v => accessor(v.get), columnNames(id))

          List(
            // column(ActionsColumnId, identity)
            //   .setCell(cell =>
            //     val thisOa = cell.getValue()
            //     val id     = thisOa.id
            //
            //     <.div(
            //       // The upload "button" needs to be a label. In order to make
            //       // the styling consistent they're all labels.
            //       <.label(
            //         Icons.Trash
            //           // ^.onClick ==> deletePrompt(props, client, thisOa, assignments.get(id).orEmpty)
            //       ).withTooltip("Delete attachment")
            //     )
            //   )
            //   .setEnableSorting(false),
            column(FileNameColumnId, ObsAttachment.fileName.get)
              .setCell(_.value.value)
              .sortableBy(_.value.toUpperCase)
          )
      )
      // Rows
      .useMemoBy((props, _, _, _, _) => props.obsAttachments.reuseByValue)((_, _, _, _, _) =>
        _.value.toListOfViews.map(_._2)
      )
      .useReactTableBy((prop, _, _, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.get.id.toString)
        )
      )
      .render { (p, ctx, odbRestClient, action, _, _, table) =>
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
              Icons.Files.withFixedWidth(true),
              "Attachments",
              <.label(
                PrimeStyles.ButtonText |+| ExploreStyles.FinderChartsButton,
                ^.htmlFor := "attachment-upload",
                Icons.FileArrowUp.withBorder(true).withFixedWidth(true)
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
            Divider(),
            PrimeTable(
              table,
              striped = true,
              compact = Compact.Very,
              emptyMessage = <.div("No observation attachments uploaded"),
              headerMod = ExploreStyles.FinderChartsTableHeader,
              tableMod = ExploreStyles.FinderChartsTable
            )
          )
        )
      }
}
