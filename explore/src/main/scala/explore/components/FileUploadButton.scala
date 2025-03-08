// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.attachments.Action
import explore.attachments.ObsAttachmentUtils
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.syntax.all.*
import explore.utils.OdbRestClient
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Program
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.MessageItem
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.ui.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import org.scalajs.dom.HTMLInputElement

case class FileUploadButton(
  programId:      Program.Id,
  attachments:    View[AttachmentList],
  attachmentType: AttachmentType,
  onFileUploaded: Attachment.Id => Callback,
  disabled:       Boolean,
  authToken:      Option[NonEmptyString]
) extends ReactFnProps(FileUploadButton):
  private val readonly: Boolean = disabled || authToken.isEmpty

object FileUploadButton
    extends ReactFnComponent[FileUploadButton](props =>
      for
        ctx           <- useContext(AppContext.ctx)
        odbRestClient <- useMemo(props.authToken):
                           _.map(OdbRestClient[IO](ctx.environment, _))
        ref           <- useRefToVdom[HTMLInputElement]
        id            <- useId
        msgItem       <- useMemo(id): id =>
                           // We need a stable reference for toastCtx to be able to remove the message.
                           MessageItem(
                             id = s"msg-$id",
                             content = Action.Insert.msg,
                             clazz = LucumaStyles.Toast,
                             sticky = true
                           )
        action        <- useStateView(Action.None)
        _             <-
          useEffectWithDeps(action.get): action =>
            import ctx.given
            action match
              case Action.Insert => ToastCtx[IO].showToast(msgItem)
              case Action.None   => ToastCtx[IO].removeToast(msgItem)
              case _             => IO.unit
      yield
        import ctx.given

        def uploadFile(e: ReactEventFromInput) =
          odbRestClient.value.foldMap: client =>
            ObsAttachmentUtils.onInsertFileSelected(
              props.programId,
              props.attachments,
              props.attachmentType,
              client,
              action,
              props.onFileUploaded
            )(e)

        React.Fragment(
          <.label(^.htmlFor := s"attachment-upload-$id")(
            Button(
              icon = Icons.FileArrowUp.withFixedWidth(true),
              onClick = ref.get.map(_.foreach(_.click())),
              disabled = props.readonly,
              tooltip = s"Upload new ${props.attachmentType.longName}",
              tooltipOptions = TooltipOptions(
                position = Tooltip.Position.Bottom
              )
            ).mini.compact
          ),
          odbRestClient.value.map: client =>
            <.input.withRef(ref)(
              ExploreStyles.FileUpload,
              ^.tpe      := "file",
              ^.onChange ==> uploadFile,
              ^.id       := s"attachment-upload-$id",
              ^.name     := "file",
              ^.accept   := props.attachmentType.accept,
              ^.readOnly := props.readonly
            )
        )
    )
