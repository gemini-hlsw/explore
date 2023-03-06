// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.effect.IO
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.components.ExploreCopy
import explore.components.ui.ExploreStyles
import explore.model.ApiKey
import explore.model.AppContext
import explore.model.display.given
import explore.model.enums.RoleType
import explore.model.reusability.given
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import org.http4s.Credentials
import org.http4s.Headers
import org.http4s.headers.Authorization
import org.http4s.syntax.all.*
import org.typelevel.ci.*
import org.typelevel.log4cats.Logger
import queries.common.SSOQueriesGQL.UserQuery.{Data => SSOUser}
import queries.common.SSOQueriesGQL.*
import queries.schemas.SSO
import react.common.ReactFnProps
import react.primereact.*

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

  private object IsActive extends NewType[Boolean]
  private type IsActive = IsActive.Type

  private object NewKey extends NewType[Option[String]]
  private type NewKey = NewKey.Type

  private val ColDef = ColumnDef[ApiKey]

  private val ActionsColumnId: ColumnId = ColumnId("actions")
  private val IdColumnId: ColumnId      = ColumnId("id")
  private val RoleColumnId: ColumnId    = ColumnId("role")

  private def deleteKey(key: String, active: View[IsActive], newKey: View[NewKey])(using
    TransactionalClient[IO, SSO],
    Logger[IO]
  ) =
    ConfirmDialog.confirmDialog(
      message = <.div(
        s"This action will delete the key with id: $key. This action cannot be reversed."
      ),
      header = "Key delete",
      acceptLabel = "Yes, delete",
      position = DialogPosition.Top,
      accept = (for {
        _ <- active.set(IsActive(true)).to[IO]
        _ <- DeleteApiKey.execute[IO](key)
        _ <- newKey.set(NewKey(none)).to[IO]
      } yield ()).guarantee(active.set(IsActive(false)).to[IO]).runAsync,
      acceptClass = PrimeStyles.ButtonSmall,
      rejectClass = PrimeStyles.ButtonSmall,
      icon = Icons.SkullCrossBones.withColor("red")
    )

  private def createNewKey(
    keyRoleId: String,
    active:    View[IsActive],
    newKey:    View[NewKey]
  )(using
    TransactionalClient[IO, SSO]
  ) =
    (for {
      _            <- active.set(IsActive(true)).to[IO]
      newKeyResult <- NewApiKey.execute[IO](keyRoleId)
      _            <- newKey.set(NewKey(newKeyResult.createApiKey.some)).to[IO]
    } yield ()).guarantee(active.set(IsActive(false)).to[IO]).void

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView(IsActive(false))
    .useEffectResultWithDepsBy((_, ctx, isAdding) => isAdding.get) { (_, ctx, _) => _ =>
      import ctx.given
      UserQuery.query()
    }
    .useStateView(NewKey(none)) // id fo the new role id to create
    // Columns
    .useMemoBy((_, _, isAdding, _, _) => isAdding.get)((_, ctx, isAdding, _, newKey) =>
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
                  severity = Button.Severity.Secondary,
                  onClick = deleteKey(keyId, isAdding, newKey),
                  tooltip = s"Delete key $keyId"
                ).mini.compact
              )
            },
            size = 35.toPx
          )
        )
    )
    // Rows
    .useMemoBy((_, _, _, user, _, _) => user.toOption.foldMap(_.user.apiKeys)) {
      (_, _, _, _, _, _) => apiKeys => apiKeys.sortBy(_.id)
    }
    .useReactTableBy((_, _, _, _, _, cols, rows) =>
      TableOptions(cols, rows, enableSorting = true, enableColumnResizing = false)
    )
    .useStateView(RoleType.Pi)
    .render { (props, ctx, active, user, newKey, _, _, table, newRoleType) =>
      import ctx.given

      potRender[SSOUser] { ssoUser =>
        val id   = ssoUser.user.id
        val name = s"${ssoUser.user.givenName.orEmpty} ${ssoUser.user.familyName.orEmpty}"
        val role = ssoUser.role.`type`.shortName

        val allRoles = ssoUser.user.roles

        val closeButton =
          props.onClose.fold(none)(cb =>
            Button(label = "Cancel",
                   icon = Icons.Close,
                   severity = Button.Severity.Danger,
                   onClick = cb
            ).small.compact.some
          )

        val unsupportedRoles = Enumerated[RoleType].all.filterNot { rt =>
          allRoles.exists(r => r.`type` === rt)
        }
        val currentKeyRoleId = allRoles.find(_.`type` === newRoleType.get).map(_.id)

        React.Fragment(
          Divider(),
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
          newKey.get.value.map { k =>
            <.div(LucumaStyles.FormColumnCompact)(
              <.label(LucumaStyles.FormFieldLabel, "Key: "),
              <.label(ExploreStyles.NewApiKey, k),
              ExploreCopy("", k, ExploreStyles.Version, ExploreStyles.VersionUncopied),
              <.label(ExploreStyles.NewApiKeyLabel, "This API key won't be displayed again.")
            )
          },
          <.div(ExploreStyles.ProgramsPopupFauxFooter)(
            Button(
              label = "New Key with role: ",
              icon = Icons.New,
              severity = Button.Severity.Success,
              disabled = active.get.value,
              loading = active.get.value,
              onClick =
                currentKeyRoleId.map(createNewKey(_, active, newKey)).map(_.runAsync).getOrEmpty
            ).small.compact,
            EnumDropdownView(
              id = "new-key-role".refined,
              exclude = unsupportedRoles.toSet,
              value = newRoleType,
              disabled = active.get.value
            )
          ),
          ConfirmDialog()
        )
      }(user)
    }
