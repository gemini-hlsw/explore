// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.attachments

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import crystal.react.reuse.*
import crystal.syntax.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditableLabel
import explore.Icons
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.Focused
import explore.model.ObsAttachmentAssignmentMap
import explore.model.Observation
import explore.model.OverviewTabTileIds
import explore.model.TargetAttachmentAssignmentMap
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import fs2.dom
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AttachmentPurpose
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.ConfirmPopup
import lucuma.react.primereact.Dialog
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.format.*
import lucuma.ui.primereact.CheckboxView
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.given
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.table.*
import lucuma.ui.utils.*
import org.scalajs.dom.File as DomFile
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

object AttachmentsTile:
  def apply(
    programId:                   Program.Id,
    userVault:                   Option[UserVault],
    obsAttachmentAssignments:    ObsAttachmentAssignmentMap,
    targetAttachmentAssignments: TargetAttachmentAssignmentMap,
    attachments:                 View[AttachmentList],
    showObsAttachments:          Boolean,
    readOnly:                    Boolean
  ) =
    userVault
      .map: vault =>
        val authToken = vault.token
        Tile(
          OverviewTabTileIds.AttachmentsId.id,
          s"Attachments (${count(attachments, showObsAttachments)})",
          Action.None
        )(
          Body(
            programId,
            authToken,
            obsAttachmentAssignments,
            targetAttachmentAssignments,
            attachments,
            showObsAttachments,
            readOnly,
            _
          ),
          (s, _) =>
            Title(
              programId,
              authToken,
              attachments,
              showObsAttachments,
              readOnly,
              s
            )
        )
      .getOrElse(
        Tile(OverviewTabTileIds.AttachmentsId.id, "Attachments", hidden = true)(_ => EmptyVdom)
      )

  private def includedPurposes(showObsAttachments: Boolean) =
    if (showObsAttachments)
      Set(AttachmentPurpose.Observation, AttachmentPurpose.Target)
    else Set(AttachmentPurpose.Target)

  private def count(attachments: View[AttachmentList], showObsAttachments: Boolean) =
    val included = includedPurposes(showObsAttachments)
    attachments.get.count(_._2.isForPurposes(included))

  private case class Body(
    pid:                         Program.Id,
    authToken:                   NonEmptyString,
    obsAttachmentAssignments:    ObsAttachmentAssignmentMap,
    targetAttachmentAssignments: TargetAttachmentAssignmentMap,
    attachments:                 View[AttachmentList],
    showObsAttachments:          Boolean,
    readOnly:                    Boolean,
    action:                      View[Action]
  ) extends ReactFnProps(Body.component)

  private object Body extends ObsAttachmentUtils:
    type Props = Body

    case class TableMeta(
      client:            OdbRestClient[IO],
      obsAssignments:    ObsAttachmentAssignmentMap,
      targetAssignments: TargetAttachmentAssignmentMap,
      urlMap:            UrlMap,
      readOnly:          Boolean
    )

    val ColDef = ColumnDef[View[Attachment]].WithTableMeta[TableMeta]

    given Reusability[UrlMap]                     = Reusability.map
    given Reusability[ObsAttachmentAssignmentMap] = Reusability.map

    def updateAttachment(
      attachments: View[AttachmentList],
      client:      OdbRestClient[IO],
      att:         Attachment,
      files:       List[DomFile]
    )(using ToastCtx[IO]): IO[Unit] =
      files.headOption
        .map(f =>
          checkFileSize(f) {
            val name = NonEmptyString.unsafeFrom(f.name)
            client
              .updateAttachment(
                att.id,
                name,
                att.description,
                dom.readReadableStream(IO(f.stream()))
              ) *>
              IO.now()
                .flatMap { now =>
                  attachments
                    .mod(
                      _.updatedWith(att.id)(
                        _.map(
                          _.copy(
                            fileName = name,
                            updatedAt = Timestamp.unsafeFromInstantTruncated(now),
                            checked = false
                          )
                        )
                      )
                    )
                    .toAsync
                }
                .toastErrors
          }
        )
        .orEmpty

    def deleteAttachment(
      attachments: View[AttachmentList],
      client:      OdbRestClient[IO],
      aid:         Attachment.Id
    )(using ToastCtx[IO]): IO[Unit] =
      attachments.mod(_.removed(aid)).toAsync *>
        client.deleteAttachment(aid).toastErrors

    def deletePrompt(
      attachments: View[AttachmentList],
      client:      OdbRestClient[IO],
      att:         Attachment,
      obsIds:      SortedSet[Observation.Id],
      targetIds:   SortedSet[Target.Id]
    )(
      e:           ReactMouseEvent
    )(using Logger[IO], ToastCtx[IO]): Callback =
      // will only be assigned to obs or targets, not both
      val msg =
        if (obsIds.nonEmpty) s"It is assigned to ${obsIds.size} observations. "
        else if (targetIds.nonEmpty) s"It is assigned to ${targetIds.size} targets. "
        else ""
      ConfirmPopup
        .confirmPopup(
          e.currentTarget.domAsHtml,
          s"Delete attachment? ${msg}This action is not undoable.",
          acceptLabel = "Delete",
          rejectLabel = "Cancel",
          accept = deleteAttachment(attachments, client, att.id).runAsync
        )
        .show

    def columns(ctx: AppContext[IO], props: Props) = {
      import ctx.given

      def column[V](
        id:       ColumnId,
        accessor: Attachment => V
      ): ColumnDef.Single.WithTableMeta[View[Attachment], V, TableMeta] =
        ColDef(id, v => accessor(v.get), columnNames(id))

      def goToObs(obsId: Observation.Id): Callback =
        ctx.pushPage((AppTab.Observations, props.pid, Focused.singleObs(obsId)).some)

      def obsUrl(obsId: Observation.Id): String =
        ctx.pageUrl((AppTab.Observations, props.pid, Focused.singleObs(obsId)).some)

      def goToTarget(targetId: Target.Id): Callback =
        ctx.pushPage((AppTab.Targets, props.pid, Focused.target(targetId)).some)

      def targetUrl(targetId: Target.Id): String =
        ctx.pageUrl((AppTab.Targets, props.pid, Focused.target(targetId)).some)

      List(
        column(ActionsColumnId, identity)
          .withCell: cell =>
            cell.table.options.meta.map: meta =>
              val thisAtt: Attachment = cell.value
              val id: Attachment.Id   = thisAtt.id

              def onUpdateFileSelected(e: ReactEventFromInput): Callback =
                val files = e.target.files.toList
                (Callback(e.target.value = null) *>
                  updateAttachment(props.attachments, meta.client, thisAtt, files)
                    .switching(props.action.async, Action.Replace, Action.None)
                    .runAsync)
                  .when_(files.nonEmpty)

              val targetAssignments = meta.targetAssignments.get(id).orEmpty
              val deleteTooltip     =
                if (targetAssignments.nonEmpty)
                  "Attachment must be removed from all targets before it can be deleted."
                else "Delete attachment"

              <.div(
                // The upload "button" needs to be a label. In order to make
                // the styling consistent they're all labels.
                <.span( // need to wrap it in a span show tooltip shows even when "disabled"
                  <.label(
                    tableLabelButtonClasses |+|
                      Css("p-disabled").when_(targetAssignments.nonEmpty),
                    Icons.Trash,
                    ^.onClick ==> { e =>
                      if (targetAssignments.nonEmpty) Callback.empty
                      else
                        deletePrompt(
                          props.attachments,
                          meta.client,
                          thisAtt,
                          meta.obsAssignments.get(id).orEmpty,
                          meta.targetAssignments.get(id).orEmpty
                        )(e)
                    }
                  )
                ).withTooltip(deleteTooltip).unless(meta.readOnly),
                <.label(
                  tableLabelButtonClasses,
                  ^.htmlFor := s"attachment-replace-$id",
                  Icons.FileArrowUp
                ).withTooltip(
                  tooltip = s"Upload replacement file",
                  placement = Placement.Right
                ).unless(meta.readOnly),
                <.input(
                  ExploreStyles.FileUpload,
                  ^.tpe    := "file",
                  ^.onChange ==> onUpdateFileSelected,
                  ^.id     := s"attachment-replace-$id",
                  ^.name   := "file",
                  ^.accept := thisAtt.attachmentType.accept
                ).unless(meta.readOnly),
                meta.urlMap
                  .get(thisAtt.toMapKey)
                  .foldMap:
                    case Pot.Ready(url) =>
                      <.a(Icons.FileArrowDown, ^.href := url, tableLabelButtonClasses)
                        .withTooltip("Download File")
                    case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
                    case Pot.Error(t)   =>
                      <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)
              )
          .withEnableSorting(false),
        column(FileNameColumnId, Attachment.fileName.get)
          .withCell(_.value.value)
          .sortableBy(_.value.toUpperCase),
        column(AttachmentTypeColumnId, Attachment.attachmentType.get)
          .withCell(_.value.shortName),
        column(SizeColumnId, Attachment.fileSize.get)
          .withCell(cell =>
            // The fileSize will always be > 0, the api should be changed to reflect this
            NonNegLong.from(cell.value).toOption.map(_.toHumanReadableByteCount).orEmpty
          ),
        column(LastUpdateColumnId, Attachment.updatedAt.get)
          .withCell(cell => GppDateFormatter.format(cell.value.toLocalDateTime)),
        column(AssignmentsColumnId, identity)
          .withCell: cell =>
            cell.table.options.meta.map: meta =>
              val att = cell.value
              val id  = att.id
              att.attachmentType.purpose match
                case AttachmentPurpose.Observation =>
                  <.span(
                    meta.obsAssignments
                      .get(id)
                      .orEmpty
                      .toList
                      .map(obsId =>
                        <.a(
                          ^.href := obsUrl(obsId),
                          ^.onClick ==> (_.preventDefaultCB >> goToObs(obsId)),
                          obsId.toString
                        )
                      )
                      .mkReactFragment(", ")
                  )
                case AttachmentPurpose.Target      =>
                  <.span(
                    meta.targetAssignments
                      .get(id)
                      .orEmpty
                      .toList
                      .map(targetId =>
                        <.a(
                          ^.href := targetUrl(targetId),
                          ^.onClick ==> (_.preventDefaultCB >> goToTarget(targetId)),
                          targetId.toString
                        )
                      )
                      .mkReactFragment(", ")
                  )
                case AttachmentPurpose.Proposal    => EmptyVdom
          .withEnableSorting(false),
        ColDef(
          DescriptionColumnId,
          _.withOnMod(oa =>
            ctx.odbApi
              .updateAttachmentDescription(oa.id, oa.description)
              .runAsync
          ).zoom(Attachment.description),
          columnNames(DescriptionColumnId),
          cell =>
            cell.table.options.meta.map: meta =>
              EditableLabel.fromView(
                value = cell.value,
                addButtonLabel = ("Add description": VdomNode).reuseAlways,
                textClass = ExploreStyles.AttachmentName,
                inputClass = ExploreStyles.AttachmentNameInput,
                editButtonTooltip = "Edit description".some,
                deleteButtonTooltip = "Delete description".some,
                okButtonTooltip = "Accept".some,
                discardButtonTooltip = "Discard".some,
                readonly = meta.readOnly
              )
        )
          .sortableBy(_.get.map(_.value.toUpperCase).orEmpty),
        ColDef(
          CheckedColumnId,
          identity,
          columnNames(CheckedColumnId),
          cell =>
            cell.table.options.meta.map: meta =>
              CheckboxView(
                id = NonEmptyString.unsafeFrom(s"checked-${cell.value.get.id}"),
                value = cell.value
                  .withOnMod(oa =>
                    ctx.odbApi
                      .updateAttachmentChecked(oa.id, oa.checked)
                      .runAsync
                  )
                  .zoom(Attachment.checked),
                label = "",
                disabled = meta.readOnly
              )
        ).sortableBy(_.get.checked)
      )
    }

    private val component =
      ScalaFnComponent[Props](props =>
        for {
          ctx     <- useContext(AppContext.ctx)
          client  <- useMemo(props.authToken): token =>
                       OdbRestClient[IO](ctx.environment, token) // This could be in the shared state
          columns <- useMemo(())(_ => columns(ctx, props))
          rows    <- useMemo((props.showObsAttachments, props.attachments.reuseByValue)):
                       (showObsAttachments, attachments) =>
                         val included = includedPurposes(showObsAttachments)
                         attachments.value.toListOfViews
                           .map(_._2)
                           .filter(_.get.isForPurposes(included))
          urlMap  <- useStateView[UrlMap](Map.empty)
          _       <- useEffectWithDeps(rows): attachments =>
                       val allCurrentKeys = attachments.value.map(_.get.toMapKey).toSet
                       val newOas         = allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

                       val updateUrlMap =
                         urlMap.mod { umap =>
                           val filteredMap = umap.filter((k, _) => allCurrentKeys.contains(k))
                           newOas.foldRight(filteredMap)((key, m) => m.updated(key, pending))
                         }.toAsync
                       val getUrls      =
                         newOas.traverse_(key => getAttachmentUrl(client, key, urlMap))

                       updateUrlMap *> getUrls
          table   <- useReactTable:
                       TableOptions(
                         columns,
                         rows,
                         getRowId = (row, _, _) => RowId(row.get.id.toString),
                         meta = TableMeta(client,
                                          props.obsAttachmentAssignments,
                                          props.targetAttachmentAssignments,
                                          urlMap.get,
                                          props.readOnly
                         )
                       )
        } yield React.Fragment(
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            emptyMessage = <.div("No observation attachments uploaded"),
            tableMod = ExploreStyles.AttachmentsTable
          ),
          ConfirmPopup(),
          Dialog(
            onHide = Callback.empty,
            visible = props.action.get != Action.None,
            header = props.action.get.msg,
            blockScroll = true,
            modal = true,
            dismissableMask = false,
            closable = false,
            closeOnEscape = false,
            showHeader = true
          )("Please wait...")
        )
      )

  private case class Title(
    pid:                Program.Id,
    authToken:          NonEmptyString,
    attachments:        View[AttachmentList],
    showObsAttachments: Boolean,
    readOnly:           Boolean,
    action:             View[Action]
  ) extends ReactFnProps(Title.component)

  private object Title extends ObsAttachmentUtils:

    val component = ScalaFnComponent[Title](props =>
      for {
        ctx           <- useContext(AppContext.ctx)
        client        <- useMemo(props.authToken): token =>
                           OdbRestClient[IO](ctx.environment, token)
        excludedTypes <-
          useMemo(props.showObsAttachments): showObsAttachments =>
            if (showObsAttachments)
              Enumerated[AttachmentType]
                .notForPurposes(AttachmentPurpose.Target, AttachmentPurpose.Observation)
                .toSet
            else Enumerated[AttachmentType].notForPurposes(AttachmentPurpose.Target).toSet
        newAttType    <- useStateView:
                           Enumerated[AttachmentType].without(excludedTypes).head
        _             <- useEffectWithDeps(excludedTypes): excluded =>
                           if (excluded.contains(newAttType.get))
                             newAttType.set(Enumerated[AttachmentType].without(excluded).head)
                           else Callback.empty
      } yield
        import ctx.given

        if (props.readOnly) EmptyVdom
        else
          <.div(
            ExploreStyles.TableSelectionToolbar,
            EnumDropdownView(
              id = "attachment-type".refined,
              value = newAttType,
              exclude = excludedTypes,
              clazz = ExploreStyles.FlatFormField |+| ExploreStyles.AttachmentsTableTypeSelect
            ),
            <.label(
              LabelButtonClasses,
              ^.htmlFor := "attachment-upload",
              Icons.FileArrowUp
            ).withTooltip(
              tooltip = s"Upload new ${newAttType.get.shortName} attachment",
              placement = Placement.Right
            ),
            <.input(
              ExploreStyles.FileUpload,
              ^.tpe    := "file",
              ^.onChange ==> onInsertFileSelected(
                props.pid,
                props.attachments,
                newAttType.get,
                client,
                props.action
              ),
              ^.id     := "attachment-upload",
              ^.name   := "file",
              ^.accept := newAttType.get.accept
            )
          )
    )
