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

case class UserPreferencesPopup(
  // vault:                Option[UserVault],
  onClose: Option[Callback] = none
) extends ReactFnProps(UserPreferencesPopup.component)

object UserPreferencesPopup:
  private type Props = UserPreferencesPopup

  private object IsOpen extends NewType[Boolean]
  private type IsOpen = IsOpen.Type

  private object OpCounter extends NewType[Int]
  private type OpCounter = OpCounter.Type

  private object IsAdding extends NewType[Boolean]
  private type IsAdding = IsAdding.Type

  private val ColDef = ColumnDef[ApiKey]

  private val ActionsColumnId: ColumnId = ColumnId("actions")
  private val IdColumnId: ColumnId      = ColumnId("id")
  private val RoleColumnId: ColumnId    = ColumnId("role")

  private def createNewKey(newRoleType: RoleType, adding: View[IsAdding])(using
    TransactionalClient[IO, SSO]
  ) =
    (adding.set(IsAdding(true)).to[IO] *>
      NewApiKey.execute[IO]("r-103")).guarantee(adding.set(IsAdding(false)).to[IO]).void

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(IsOpen(true))
    .useState(OpCounter(0))
    .useStateView(IsAdding(false))
    .useEffectResultWithDepsBy((_, ctx, _, _, isAdding) => isAdding.get) { (_, ctx, _, _, _) => _ =>
      import ctx.given
      // val tc = ctx.given_TransactionalClient_F_SSO.withExtraHeaders(
      //   Headers(
      //     Authorization(
      //       Credentials.Token(
      //         ci"Bearer",
      //         "113.e5d16c3d8f102578b39b27817fe92b1c669c4935f0db89e0319b1af65837e5b268b359645b455bc5d61683c06a9ee67b"
      //       )
      //     )
      //   )
      // )
      UserQuery.query()
    }
    // Columns
    .useMemoBy((_, _, isOpen, _, _, _) => isOpen.value)((_, _, _, _, _, _) =>
      _ =>
        // import ctx.given

        List(
          ColDef(
            ActionsColumnId,
            identity[ApiKey] _,
            "Actions",
            cell = { cell =>
              val keyId = cell.value.id
              <.div(
                Button(
                  icon = Icons.Trash,
                  severity = Button.Severity.Secondary,
                  // disabled = currentProgramId.exists(_ === programId) || programCount < 2,
                  // onClick = deleteProgram(cell.value).runAsync,
                  tooltip = "Delete key"
                ).mini.compact
              )
            }
          ).sortableBy(_.id),
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
          ).sortable
        )
    )
    // Rows
    .useMemoBy((_, _, _, _, _, user, _) => user.toOption.foldMap(_.user.apiKeys)) {
      (_, _, _, _, _, _, _) => apiKeys => apiKeys
    }
    .useReactTableBy((_, _, _, _, _, _, cols, rows) =>
      TableOptions(cols, rows, enableSorting = true, enableColumnResizing = false)
    )
    .useStateView(RoleType.Pi)
    .render { (props, ctx, isOpen, counter, adding, user, _, _, table, newRoleType) =>
      import ctx.given
      potRender[SSOUser] { user =>
        val id   = user.user.id
        val name = s"${user.user.givenName.orEmpty} ${user.user.familyName.orEmpty}"
        val role = user.role.`type`.shortName

        val onHide = props.onClose.map(oc => isOpen.setState(IsOpen(false)) >> oc)

        val closeButton =
          props.onClose.fold(none)(cb =>
            Button(label = "Cancel",
                   icon = Icons.Close,
                   severity = Button.Severity.Danger,
                   onClick = cb
            ).small.compact.some
          )

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
                // clazz = ExploreStyles.BrightnessesTableUnitsDropdown
              )
              // closeButton
            )
          )
        )
      }(user)
    }
