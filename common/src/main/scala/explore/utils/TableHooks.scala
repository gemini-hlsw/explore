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

  def useTablePreferencesLoad[T] =
    CustomHook[
      (Option[User.Id], AppContext[IO], TableId, List[ColumnDef[T, ?]], List[TableColumnPref])
    ]
      .useStateViewWithReuseBy(_._5)
      // Read prefs from the db
      .useEffectWithDepsBy(_.hook1) { hook => prefs =>
        val (userId, ctx, tid, cols, _) = hook.input
        import ctx.given

        val allColumns =
          cols
            .map(col => TableColumnPref(ColumnId(col.id), visible = true, None))
            .withStored(prefs.get)

        TableColumns
          .queryColumns[IO](userId, tid)
          .flatMap(r =>
            prefs
              .set(allColumns.withStored(r.lucumaTableColumnPreferences).sortBy(_.columnId.value))
              .to[IO]
          )
          .runAsyncAndForget
      }
      // Return a view that will save on mod
      .buildReturning(hook =>
        val (userId, ctx, tid, cols, _) = hook.input
        import ctx.given

        hook.hook1.withOnMod { l =>
          TableColumns
            .storeColumns[IO](userId, TableId.ConstraintsSummary, l)
            .runAsyncAndForget
        }.reuseByValue
      )

  def useTablePreferencesStore[A] =
    CustomHook[(ReuseView[List[TableColumnPref]], Table[A])]
      // If prefs change update the table ui
      .useEffectWithDepsBy((prefs, table) => prefs) { (_, table) => prefs =>
        Callback {
          table.setColumnVisibility(prefs.get.hiddenColumnsDictionary)
          table.setSorting(toSortingRules(prefs.get.sortingColumns))
        }
      }
      // If sorting changes update the prefs (and save in db as a side effect)
      .useEffectWithDepsBy((_, table) => fromTableState(table.getState()))((prefs, table) =>
        rules =>
          prefs
            .mod(_.map {
              case t @ TableColumnPref(cid, _, _) if rules.find(_._1 === cid.value).isDefined =>
                t.copy(sorting =
                  rules.find(_._1 === cid.value).map(r => SortDirection.fromBoolean(r._2))
                )
              case t                                                                          =>
                t.copy(sorting = none)
            })
      )
      // This should ideally be just `build` but something breaks on the subsequent hooks if you do
      .buildReturning(_._2)
