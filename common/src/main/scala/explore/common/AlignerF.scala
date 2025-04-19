// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.MonadError
import crystal.react.*
import explore.undo.UndoSetter
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.util.Effect
import monocle.Lens
import monocle.Optional
import monocle.Prism
import org.typelevel.log4cats.Logger

/**
 * Provides a `View` into a model with the ability to perform changes remotely whenever the `View`
 * is modified. Additionally, operations are undoable.
 *
 * `A` is the model type, whereas `T` is the type of a delta structure to be sent to the remote
 * server with only the necessary changes. For example, it can be one of the `*Input` types of the
 * API.
 *
 * We can `zoom` into a property of the model while providing a parallel way to zoom into the delta
 * structure. Potential modifications to the the delta structure are stored in a coyoneda-like
 * fashion, but are not executed until the model is modified.
 *
 * The idea is to keep code to edit the model as simple as possible, while providing a way to
 * compose components that edit different parts of the model in the same undo context.
 */
trait AlignerF[F[_], B, S]:
  protected type _A // Base model type
  protected type _T // Base delta structure type

  protected val _undoCtx: UndoSetter[_A] // Base model instance within an `UndoContext`
  protected val _remoteBaseInput: _T     // Base delta structure instance
  protected val _onMod: _T => F[Unit]    // Effect to send the delta structure to the remote server

  protected val _modelGet: _A => B               // Model drill-down getter function
  protected val _modelMod: (B => B) => _A => _A  // Model drill-down modifier function
  protected val _remoteMod: (S => S) => _T => _T // Delta structure drill-down modifier function

  /** Get the value of the model. */
  def get: B = _modelGet(_undoCtx.get)

  /**
   * Build an undoable `View` at the current level, specifying a function that modifies the delta
   * structure so that model changes are added to the structure.
   *
   * The `View` should be built at the last possible moment in order to maximize the granularity of
   * undo operations and remote changes.
   */
  def viewMod(
    toInput: B => S => S
  )(using MonadError[F, Throwable], Effect.Dispatch[F], Logger[F]): View[B] =
    _undoCtx
      .undoableView(_modelGet, _modelMod)
      .withOnMod(value => _onMod(_remoteMod(toInput(value))(_remoteBaseInput)).runAsync)

  /**
   * Build an undoable `View` at the current level, specifying a conversion from the model to the
   * delta structure.
   *
   * The `View` should be built at the last possible moment in order to maximize the granularity of
   * undo operations and remote changes.
   */
  def view(
    toInput: B => S
  )(using MonadError[F, Throwable], Effect.Dispatch[F], Logger[F]): View[B] =
    viewMod(b => _ => toInput(b))

  /**
   * Drill-down specifying model getter, model modification and delta structure modification
   * functions.
   */
  def zoom[C, U](
    modelGet:  B => C,
    modelMod:  (C => C) => B => B,
    remoteMod: (U => U) => S => S
  ): AlignerF[F, C, U] =
    AlignerF(
      _undoCtx,
      _remoteBaseInput,
      _onMod,
      _modelGet.andThen(modelGet),
      _modelMod.compose(modelMod),
      _remoteMod.compose(remoteMod)
    )

  /** Drill-down specifying model `Lens` and delta structure modification function. */
  def zoom[C, U](
    lens:      Lens[B, C],
    remoteMod: (U => U) => S => S
  ): AlignerF[F, C, U] =
    zoom(lens.get, lens.modify, remoteMod)

  /**
   * Optional drill-down specifying model optional getter, model modification and delta structure
   * modification functions.
   */
  def zoomOpt[C, U](
    modelGetOpt: B => Option[C],
    modelMod:    (C => C) => B => B,
    remoteMod:   (U => U) => S => S
  ): Option[AlignerF[F, C, U]] =
    modelGetOpt(get).map(_ => zoom(modelGetOpt.andThen(_.get), modelMod, remoteMod))

  /**
   * Optional drill-down specifying model `Prism` and delta structure modification function.
   */
  def zoomOpt[C, U](
    prism:     Prism[B, C],
    remoteMod: (U => U) => S => S
  ): Option[AlignerF[F, C, U]] =
    zoomOpt(prism.getOption, prism.modify, remoteMod)

  /**
   * Optional drill-down specifying model `Optional` and delta structure modification function.
   */

  def zoomOpt[C, U](
    optional:  Optional[B, C],
    remoteMod: (U => U) => S => S
  ): Option[AlignerF[F, C, U]] =
    zoomOpt(optional.getOption, optional.modify, remoteMod)

  /**
   * Turn an `AlignerF[F, Option[B], S]` into an `Option[AlignerF[F, B, S]]
   */
  def toOption[C](implicit ev: B =:= Option[C], ev2: Option[C] =:= B): Option[AlignerF[F, C, S]] =
    get.map { _ =>
      val bToC: B => C                 = _.get
      val modelMod: (C => C) => B => B = f => _.map(f)
      zoom(bToC, modelMod, identity[S => S])
    }

object AlignerF:

  /**
   * Build a `AlignerF` for property `B` of base model `A` and property `S` of delta structure `T`
   */
  def apply[F[_], A, B, S, T](
    undoCtx:         UndoSetter[A],
    remoteBaseInput: T,
    onMod:           T => F[Unit],
    modelGet:        A => B,
    modelMod:        (B => B) => A => A,
    remoteMod:       (S => S) => T => T
  ): AlignerF[F, B, S] =
    new AlignerF[F, B, S] {
      protected type _A = A
      protected type _T = T

      protected val _undoCtx: UndoSetter[A] = undoCtx
      protected val _remoteBaseInput: T     = remoteBaseInput
      protected val _onMod: T => F[Unit]    = onMod

      protected val _modelGet: A => B              = modelGet
      protected val _modelMod: (B => B) => A => A  = modelMod
      protected val _remoteMod: (S => S) => T => T = remoteMod
    }

  /** Build a base `AlignerF` for model `A` and delta structure `T` */
  def apply[F[_], A, T](
    undoCtx:         UndoSetter[A],
    remoteBaseInput: T,
    onMod:           T => F[Unit]
  ): AlignerF[F, A, T] =
    AlignerF[F, A, A, T, T](undoCtx, remoteBaseInput, onMod, identity, identity, identity)

  given [F[_], B: Reusability, S]: Reusability[AlignerF[F, B, S]] =
    Reusability.by(_.get)
