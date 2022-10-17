// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.syntax.ui

import cats.*
import cats.syntax.all.*
import crystal.react.implicits.*
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.model.Constants
import explore.model.TableColumnPref
import explore.utils.*
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.util.Effect
import lucuma.ui.forms.ExternalValue
import lucuma.ui.forms.FormInputEV
import org.scalablytyped.runtime.StringDictionary
import org.scalajs.dom.Window
import org.typelevel.log4cats.Logger
import react.common.Css

import scala.scalajs.js
import scala.scalajs.js.UndefOr

extension (self: Window)
  def canFitTwoPanels: Boolean =
    self.innerWidth <= Constants.TwoPanelCutoff

extension [EV[_], A, B](input: FormInputEV[EV, Option[A]])
  def clearable(using ev: ExternalValue[EV], ev3: Eq[A]) =
    input.copy(icon = clearInputIcon[EV, A](input.value))

  // When an icon is added to a FormInputEV, SUI adds extra padding on the right to make
  // space for the icon. However, with some layouts this can cause resizing issues, so this
  // method removes that extra padding. See `clearInputIcon` for more details.
  def clearableNoPadding(using ev: ExternalValue[EV], ev3: Eq[A]) = {
    val newClazz: UndefOr[Css] =
      input.clazz.fold(ExploreStyles.ClearableInputPaddingReset)(
        _ |+| ExploreStyles.ClearableInputPaddingReset
      )
    input.copy(icon = clearInputIcon[EV, A](input.value), clazz = newClazz)
  }

extension [EV[_], A, B](input: InputWithUnits[EV, Option[A]])
  def clearable(using ev: ExternalValue[EV], ev3: Eq[A]) =
    input.copy(icon = clearInputIcon[EV, A](input.value))

  // When an icon is added to a FormInputEV, SUI adds extra padding on the right to make
  // space for the icon. However, with some layouts this can cause resizing issues, so this
  // method removes that extra padding. See `clearInputIcon` for more details.
  def clearableNoPadding(using ev: ExternalValue[EV], ev3: Eq[A]) = {
    val newClazz = input.clazz |+| ExploreStyles.ClearableInputPaddingReset
    input.copy(icon = clearInputIcon[EV, A](input.value), clazz = newClazz)
  }

extension [A](c: js.UndefOr[A => Callback])
  def toJs: js.UndefOr[js.Function1[A, Unit]] = c.map(x => (a: A) => x(a).runNow())

extension [F[_]: MonadThrow](c: Logger[F])
  def pdebug[T](a: T): F[Unit] = c.debug(_root_.pprint.apply(a).render)

  def pdebugCB[T](a: T)(using Effect.Dispatch[F]): Callback =
    c.debug(_root_.pprint.apply(a).render).runAsyncAndForget

extension [F[_]: Functor: FunctorFilter: Foldable](cols: F[TableColumnPref])
  def hiddenColumns: F[String] =
    cols.collect { case TableColumnPref(cid, false, _) => cid.value }

  def sortingColumns: F[(String, Boolean)] =
    cols.collect { case TableColumnPref(cid, _, Some(d)) => cid.value -> d.toBool }

  def hiddenColumnsDictionary: StringDictionary[Boolean] =
    StringDictionary(
      hiddenColumns.toList.map(_ -> false): _*
    )

  def withStored(storedCols: F[TableColumnPref]): F[TableColumnPref] =
    cols.map { case t @ TableColumnPref(cid, _, _) =>
      storedCols.find(_.columnId === cid).getOrElse(t)
    }
