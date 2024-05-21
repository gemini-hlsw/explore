// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.effect.IO
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import clue.js.FetchJSClient
import crystal.react.*
import crystal.react.hooks.*
import explore.DefaultErrorPolicy
import explore.Icons
import explore.common.UserPreferencesQueries.GridLayouts
import explore.components.deleteConfirmation
import explore.components.ui.ExploreStyles
import explore.model.ApiKey
import explore.model.AppContext
import explore.model.IsActive
import explore.model.display.given
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.RoleType
import lucuma.core.model.StandardRole
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.components.CopyControl
import lucuma.ui.components.SolarProgress
import lucuma.ui.primereact.*
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import lucuma.ui.table.*
import org.scalajs.dom.window
import org.typelevel.log4cats.Logger
import queries.common.SSOQueriesGQL.*
import queries.schemas.SSO

case class UserPreferencesPopup(vault: UserVault, onClose: Option[Callback] = none)
    extends ReactFnProps(UserPreferencesPopup.component)

object UserPreferencesPopup:
  private type Props = UserPreferencesPopup

  private object IsOpen extends NewType[Boolean]

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
        clazz = LucumaPrimeStyles.Dialog.Small |+| ExploreStyles.ApiKeysPopup,
        header = "User Preferences"
      )(
        UserPreferencesContent(props.vault, props.onClose)
      )
    )

case class UserPreferencesContent(vault: UserVault, onClose: Option[Callback] = none)
    extends ReactFnProps(UserPreferencesContent.component)

object UserPreferencesContent:
  private type Props = UserPreferencesContent

  private object IsCleaningTheCache extends NewType[Boolean]
  private object IsDeletingLayouts  extends NewType[Boolean]

  private object NewKey extends NewType[Option[String]]
  private type NewKey = NewKey.Type

  private val ColDef = ColumnDef[ApiKey]

  private val ActionsColumnId: ColumnId = ColumnId("actions")
  private val IdColumnId: ColumnId      = ColumnId("id")
  private val RoleColumnId: ColumnId    = ColumnId("role")

  private def createNewKey(
    keyRoleId: StandardRole.Id,
    active:    View[IsActive],
    newKey:    View[NewKey],
    vault:     UserVault
  )(using
    FetchJSClient[IO, SSO],
    Logger[IO]
  ) =
    (for {
      newKeyResult <- NewApiKey[IO].execute(keyRoleId, modParams = vault.addAuthorizationHeaderTo)
      _            <- newKey.set(NewKey(newKeyResult.createApiKey.some)).toAsync
    } yield ()).switching(active.async, IsActive(_))

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView[IsActive](IsActive(false))
    .useEffectResultWithDepsBy((_, ctx, isActive) => isActive.get) { (props, ctx, _) => _ =>
      import ctx.given
      UserQuery[IO].query(modParams = props.vault.addAuthorizationHeaderTo)
    }
    .useStateView(NewKey(none)) // id fo the new role id to create
    // Columns
    .useMemoBy((_, _, isActive, _, _) => isActive.get)((props, ctx, isActive, _, newKey) =>
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
            identity[ApiKey],
            "Delete",
            cell = { cell =>
              val keyId = cell.value.id

              val action = for {
                _ <-
                  DeleteApiKey[IO].execute(keyId, modParams = props.vault.addAuthorizationHeaderTo)
                _ <- newKey.set(NewKey(none)).toAsync
              } yield ()

              val deleteKey = deleteConfirmation(
                s"This action will delete the key with id: $keyId. This action cannot be reversed.",
                "Key delete",
                "Yes, delete",
                action,
                isActive
              )

              <.div(
                ExploreStyles.ApiKeyDelete,
                Button(
                  icon = Icons.Trash,
                  severity = Button.Severity.Secondary,
                  onClick = deleteKey,
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
    .useState(IsCleaningTheCache(false))
    .useStateView(IsDeletingLayouts(false))
    .render {
      (
        props,
        ctx,
        active,
        user,
        newKey,
        _,
        _,
        table,
        newRoleType,
        isCleaningTheCache,
        isDeletingLayouts
      ) =>
        import ctx.given

        val layoutsPopup = ToastCtx[IO].showToast(
          MessageItem(
            content = <.div(
              ExploreStyles.ExplorePromptToast,
              <.span(
                "Layouts reset. You may need to reload page for it to take effect."
              ),
              Button(size = Button.Size.Small, onClick = Callback(window.location.reload()))(
                "Reload ..."
              )
            ),
            clazz = ExploreStyles.ExploreToast,
            sticky = false
          )
        )

        user.renderPot(
          ssoUser => {
            val id   = ssoUser.user.id
            val name = s"${ssoUser.user.givenName.orEmpty} ${ssoUser.user.familyName.orEmpty}"
            val role = ssoUser.role.`type`.shortName

            val allRoles = ssoUser.user.roles

            val requestCacheClean =
              for {
                _ <- isCleaningTheCache.setState(IsCleaningTheCache(true)).toAsync
                _ <- ctx.workerClients.clearAll(
                       isCleaningTheCache.setState(IsCleaningTheCache(false)).toAsync
                     )
              } yield ()

            val cleanCacheButton =
              Button(
                label = "Clean local cache",
                disabled = isCleaningTheCache.value.value,
                loading = isCleaningTheCache.value.value,
                icon = Icons.Trash,
                onClick = requestCacheClean.runAsyncAndForget
              ).small.compact

            val deleteGridLayoutsButton =
              Button(
                label = "Reset Tile Layouts",
                disabled = isDeletingLayouts.get.value,
                loading = isDeletingLayouts.get.value,
                icon = Icons.Trash,
                tooltip = "Revert to the default tile layouts for the various tabs",
                onClick = (GridLayouts
                  .deleteLayoutsPreference(props.vault.user.id)
                  .switching(isDeletingLayouts.async,
                             IsDeletingLayouts(_)
                  ) *> layoutsPopup).runAsyncAndForget
              ).small.compact

            val unsupportedRoles = Enumerated[RoleType].all.filterNot { rt =>
              allRoles.exists(r => r.`type` === rt)
            }
            val currentKeyRoleId = allRoles.find(_.`type` === newRoleType.get).map(_.id)

            React.Fragment(
              Divider(),
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                <.label(LucumaPrimeStyles.FormFieldLabel, "ID: "),
                <.label(LucumaPrimeStyles.FormField, id.show),
                <.label(LucumaPrimeStyles.FormFieldLabel, "Name: "),
                <.label(LucumaPrimeStyles.FormField, name),
                <.label(LucumaPrimeStyles.FormFieldLabel, "Role: "),
                <.label(LucumaPrimeStyles.FormField, role)
              ),
              Divider(),
              <.h4("API Keys:"),
              <.div(ExploreStyles.ApiKeysTable)(
                PrimeAutoHeightVirtualizedTable(
                  table,
                  estimateSize = _ => 32.toPx,
                  striped = true,
                  compact = Compact.Very,
                  tableMod =
                    ExploreStyles.ApiKeysTableMod |+| ExploreStyles.ExploreTable |+| ExploreStyles.ExploreBorderTable,
                  emptyMessage = "No keys available"
                )
              ),
              Divider(),
              newKey.get.value.map { k =>
                <.div(LucumaPrimeStyles.FormColumnCompact)(
                  <.label(LucumaPrimeStyles.FormFieldLabel, "Key: "),
                  <.label(ExploreStyles.NewApiKey, k),
                  CopyControl("", k),
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
                  onClick = currentKeyRoleId
                    .map(createNewKey(_, active, newKey, props.vault))
                    .map(_.runAsync)
                    .getOrEmpty
                ).small.compact,
                EnumDropdownView(
                  id = "new-key-role".refined,
                  exclude = unsupportedRoles.toSet,
                  value = newRoleType,
                  disabled = active.get.value
                )
              ),
              Divider(),
              <.div(
                cleanCacheButton,
                deleteGridLayoutsButton
              )
            )
          },
          pendingRender = <.div(ExploreStyles.EmptyUserPreferences, SolarProgress())
        )
    }
