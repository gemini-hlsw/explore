// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.effect.IO
import cats.effect.Sync
import cats.syntax.all.*
import crystal.react.implicits.*
import explore.model.reusability.given
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.CustomHook
import japgolly.scalajs.react.util.DefaultEffects.{Async => DefaultA}
import lucuma.react.table.*
import reactST.{tanstackTableCore => raw}

import scalajs.js.JSConverters.*
import scalajs.js

case class TableOptionsWithStateStore[F[_], T](
  tableOptions: TableOptions[T],
  stateStore:   TableStateStore[F]
)

private object TableHooks:
  private given Reusability[Map[String, Boolean]] = Reusability.map

  private given Reusability[raw.mod.VisibilityState] = Reusability.by(_.toMap)

  private given Reusability[raw.mod.SortingState] =
    Reusability.by(_.toList.map(cs => (cs.id, cs.desc)))

  private given Reusability[raw.mod.TableState] =
    Reusability.by(state => (state.columnVisibility, state.sorting))

  private def hook[T] =
    CustomHook[TableOptionsWithStateStore[DefaultA, T]]
      .useReactTableBy(_.tableOptions)
      .useState(false) // prefsDone
      .useRef(false)   // canSave
      .useEffectOnMountBy((props, table, prefsDone, canSave) =>
        (props.stateStore.load() >>=
          (mod => Sync[DefaultA].delay(table.setState(mod))))
          .guarantee( // This also forces a rerender, which react-table isn't doing by just changing the state.
            prefsDone.setStateAsync(true)
          )
      )
      .useEffectWithDepsBy((_, table, _, _) => table.getState())((props, _, prefsDone, canSave) =>
        state =>
          // Don't save prefs while we are still attempting to load them or if we just loaded them.
          props.stateStore
            .save(state)
            .whenA(prefsDone.value && canSave.value)
            >> canSave.setAsync(true).whenA(prefsDone.value && !canSave.value)
      )
      .buildReturning((_, table, _, _) => table)

  sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]):
    final def useReactTableWithStateStore[T](
      options: TableOptionsWithStateStore[DefaultA, T]
    )(using
      step:    Step
    ): step.Next[raw.mod.Table[T]] =
      useReactTableWithStateStoreBy(_ => options)

    final def useReactTableWithStateStoreBy[T](
      options: Ctx => TableOptionsWithStateStore[DefaultA, T]
    )(using
      step:    Step
    ): step.Next[raw.mod.Table[T]] =
      api.customBy(ctx => hook(options(ctx)))

  final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ) extends Primary[Ctx, Step](api):
    def useReactTableWithStateStoreBy[T](
      tableDefWithOptions: CtxFn[TableOptionsWithStateStore[DefaultA, T]]
    )(implicit
      step:                Step
    ): step.Next[raw.mod.Table[T]] =
      super.useReactTableWithStateStoreBy(step.squash(tableDefWithOptions)(_))

trait TableHooks:
  import TableHooks._

  given [Ctx, Step <: HooksApi.AbstractStep]
    : Conversion[HooksApi.Primary[Ctx, Step], Primary[Ctx, Step]] =
    api => new Primary(api)

  given [Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]]
    : Conversion[HooksApi.Secondary[Ctx, CtxFn, Step], Secondary[Ctx, CtxFn, Step]] =
    api => new Secondary(api)
