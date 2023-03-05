// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.syntax.all.*
import cats.implicits.catsKernelOrderingForOrder
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import crystal.react.hooks.*
import lucuma.core.util.NewType
import explore.model.AppContext

import react.primereact.Dialog
import react.primereact.DialogPosition
import explore.components.ui.ExploreStyles
import react.common.ReactFnProps
import org.http4s.Headers
import org.typelevel.ci.*
import org.http4s.syntax.all.*
import org.http4s.headers.Authorization
import queries.common.SSOQueriesGQL.*
import queries.common.SSOQueriesGQL.UserQuery.{Data => SSOUser}
import queries.schemas.SSO
import cats.effect.IO
import org.http4s.Credentials
import lucuma.ui.primereact.LucumaStyles
import lucuma.core.syntax.display.*
import explore.model.display.given
import explore.model.reusability.given
import lucuma.ui.table.*
import crystal.react.View
import crystal.react.reuse.*
import crystal.react.implicits.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import explore.Icons
import lucuma.ui.reusability.given
import lucuma.ui.table.*
import explore.model.ApiKey
import explore.utils.*
import explore.syntax.ui.given
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.refined.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import react.primereact.Divider
import react.primereact.Button
import explore.model.enums.RoleType
import clue.TransactionalClient

case class UserPreferencesPopup(onClose: Option[Callback] = none)
    extends ReactFnProps(UserPreferencesPopup.component)

object UserPreferencesPopup:
  private type Props = UserPreferencesPopup

  private object IsOpen extends NewType[Boolean]
  private type IsOpen = IsOpen.Type

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useState(IsOpen(true))
    .render((props, isOpen) =>
      val onHide = props.onClose.map(oc => isOpen.setState(IsOpen(false)) >> oc)
      Dialog(
        visible = isOpen.value.value,
        onHide = onHide.orEmpty,
        position = DialogPosition.Top,
        closeOnEscape = props.onClose.isDefined,
        closable = props.onClose.isDefined,
        dismissableMask = props.onClose.isDefined,
        resizable = false,
        clazz = ExploreStyles.Dialog.Small |+| ExploreStyles.ApiKeysPopup,
        header = "User Preferences"
      )(
        UserPreferencesContent(props.onClose)
      )
    )

case class UserPreferencesContent(onClose: Option[Callback] = none)
    extends ReactFnProps(UserPreferencesContent.component)

object UserPreferencesContent:
  private type Props = UserPreferencesContent

  private object OpCounter extends NewType[Int]
  private type OpCounter = OpCounter.Type

  private object IsAdding extends NewType[Boolean]
  private type IsAdding = IsAdding.Type

  private val ColDef = ColumnDef[ApiKey]

  private val ActionsColumnId: ColumnId = ColumnId("actions")
  private val IdColumnId: ColumnId      = ColumnId("id")
  private val RoleColumnId: ColumnId    = ColumnId("role")

  private def deleteKey(key: String, adding: View[IsAdding])(using
    TransactionalClient[IO, SSO]
  ) =
    (adding.set(IsAdding(true)).to[IO] *>
      DeleteApiKey.execute[IO](key)).guarantee(adding.set(IsAdding(false)).to[IO]).void

  private def createNewKey(newRoleType: RoleType, adding: View[IsAdding])(using
    TransactionalClient[IO, SSO]
  ) =
    (adding.set(IsAdding(true)).to[IO] *>
      NewApiKey.execute[IO]("r-103")).guarantee(adding.set(IsAdding(false)).to[IO]).void

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(OpCounter(0))
    .useStateView(IsAdding(false))
    .useEffectResultWithDepsBy((_, ctx, _, isAdding) => isAdding.get) { (_, ctx, _, _) => _ =>
      import ctx.given
      UserQuery.query()
    }
    // Columns
    .useMemoBy((_, _, _, isAdding, _) => isAdding.get)((_, ctx, _, isAdding, _) =>
      _ =>
        import ctx.given

        List(
          ColDef(
            IdColumnId,
            _.id,
            "Id",
            _.value.toString
          ).sortable,
          ColDef(
            RoleColumnId,
            _.role.`type`,
            "Role",
            _.value.toString
          ).sortable,
          ColDef(
            ActionsColumnId,
            identity[ApiKey] _,
            "Delete",
            cell = { cell =>
              val keyId = cell.value.id
              <.div(
                ExploreStyles.ApiKeyDelete,
                Button(
                  icon = Icons.Trash,
                  clazz = ExploreStyles.BlendedButton,
                  severity = Button.Severity.Secondary,
                  onClick = deleteKey(keyId, isAdding).runAsync,
                  tooltip = "Delete key"
                ).mini.compact
              )
            },
            size = 35.toPx
          )
        )
    )
    // Rows
    .useMemoBy((_, _, _, _, user, _) => user.toOption.foldMap(_.user.apiKeys)) {
      (_, _, _, _, _, _) => apiKeys => apiKeys
    }
    .useReactTableBy((_, _, _, _, _, cols, rows) =>
      TableOptions(cols, rows, enableSorting = true, enableColumnResizing = false)
    )
    .useStateView(RoleType.Pi)
    .render { (props, ctx, counter, adding, user, _, _, table, newRoleType) =>
      import ctx.given
      potRender[SSOUser] { user =>
        val id   = user.user.id
        val name = s"${user.user.givenName.orEmpty} ${user.user.familyName.orEmpty}"
        val role = user.role.`type`.shortName

        val closeButton =
          props.onClose.fold(none)(cb =>
            Button(label = "Cancel",
                   icon = Icons.Close,
                   severity = Button.Severity.Danger,
                   onClick = cb
            ).small.compact.some
          )

        React.Fragment(
          <.div(LucumaStyles.FormColumnCompact)(
            <.label(LucumaStyles.FormFieldLabel, "ID: "),
            <.label(LucumaStyles.FormField, id),
            <.label(LucumaStyles.FormFieldLabel, "Name: "),
            <.label(LucumaStyles.FormField, name),
            <.label(LucumaStyles.FormFieldLabel, "Role: "),
            <.label(LucumaStyles.FormField, role)
          ),
          Divider(),
          <.h4("API Keys:"),
          <.div(ExploreStyles.ApiKeysTable)(
            PrimeAutoHeightVirtualizedTable(
              table,
              estimateSize = _ => 32.toPx,
              striped = true,
              compact = Compact.Very,
              tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreBorderTable,
              emptyMessage = "No keys available"
            )
          ),
          Divider(),
          <.div(ExploreStyles.ProgramsPopupFauxFooter)(
            Button(
              label = "Key with role: ",
              icon = Icons.New,
              severity = Button.Severity.Success,
              disabled = adding.get.value,
              loading = adding.get.value,
              onClick = createNewKey(newRoleType.get, adding).runAsync
            ).small.compact,
            EnumDropdownView(
              id = "new-key-role".refined,
              value = newRoleType,
              disabled = adding.get.value
            )
          )
        )
      }(user)
    }
