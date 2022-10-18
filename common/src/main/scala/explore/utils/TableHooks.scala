// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.ReuseView
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.common.UserPreferencesQueries.TableColumns
import explore.model.AppContext
import explore.model.ColumnId
import explore.model.TableColumnPref
import explore.model.enums.SortDirection
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.syntax.ui.*
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.hooks.CustomHook
import lucuma.core.model.User
import lucuma.react.table.ColumnDef
import reactST.tanstackTableCore.mod.Table
import reactST.{tanstackTableCore => raw}

import scalajs.js.JSConverters.*
import scalajs.js

trait TableHooks:
  def toSortingRules(tuples: List[(String, Boolean)]): js.Array[raw.mod.ColumnSort] =
    tuples.map { case (id, b) => raw.mod.ColumnSort(b, id) }.toJSArray

  def fromTableState(state: raw.mod.TableState): List[(String, Boolean)] =
    state.sorting.toList.map(sr => (sr.id.toString, sr.desc))

  case class TablePrefsLoadParams[F[_], A](
    userId: Option[User.Id],
    ctx:    AppContext[F],
    tid:    TableId,
    cols:   List[ColumnDef[A, ?]],
    prefs:  List[TableColumnPref]
  )

  def useTablePreferencesLoad[A] =
    CustomHook[TablePrefsLoadParams[IO, A]]
      .useStateViewWithReuseBy(_.prefs)
      // Read prefs from the db
      .useEffectWithDepsBy(_.hook1) { hook => prefs =>
        val params = hook.input
        import params.ctx.given

        val allColumns =
          params.cols
            .map(col => TableColumnPref(ColumnId(col.id), visible = true, None))
            .withStored(prefs.get)

        TableColumns
          .queryColumns[IO](params.userId, params.tid)
          .flatMap(r =>
            prefs
              .set(allColumns.withStored(r.lucumaTableColumnPreferences).sortBy(_.columnId.value))
              .to[IO]
          )
          .runAsyncAndForget
      }
      // Return a view that will save on mod
      .buildReturning(hook =>
        val params = hook.input
        import params.ctx.given

        hook.hook1.withOnMod { l =>
          TableColumns
            .storeColumns[IO](params.userId, params.tid, l)
            .runAsyncAndForget
        }.reuseByValue
      )

  case class TablePrefsStoreParams[A](prefs: ReuseView[List[TableColumnPref]], table: Table[A])

  def useTablePreferencesStore[A] =
    CustomHook[TablePrefsStoreParams[A]]
      // If prefs change update the table ui
      .useEffectWithDepsBy(_.prefs) { params => prefs =>
        Callback {
          params.table.setColumnVisibility(prefs.get.hiddenColumnsDictionary)
          params.table.setSorting(toSortingRules(prefs.get.sortingColumns))
        }
      }
      // If sorting changes update the prefs (and save in db as a side effect)
      .useEffectWithDepsBy(params => fromTableState(params.table.getState()))(params =>
        rules =>
          params.prefs
            .mod(_.map {
              case t @ TableColumnPref(cid, _, _) if rules.find(_._1 === cid.value).isDefined =>
                t.copy(sorting =
                  rules.find(_._1 === cid.value).map(r => SortDirection.fromBoolean(r._2))
                )
              case t                                                                          =>
                t.copy(sorting = none)
            })
            .when_(rules.nonEmpty)
      )
      // This should ideally be just `build` but something breaks on the subsequent hooks if you do
      .buildReturning(_._2)
