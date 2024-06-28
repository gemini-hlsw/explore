# Undo implementation

## Basics

Undo works over a model of type `M`.

Undo funcionality is provided via `UndoContext[M]`, which wraps an instance of `M`.

In order to modify a property of type `A` of the model, you use the method:
```scala
def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => F[Unit],
    onRestore: (M, A) => F[Unit]
  )(v: A): Callback
```

These 4 parameters are:
- `getter: M => A`:  Used by the undoer to read the value from the model. The obtained value is pushed to the undo stack.
- `setter: A => M => M`: Used to actually modify the model.
- `onSet: (M, A) => F[Unit]`: Effect triggered when the property is set directly or via a redo. This can be used to modify a persistent or remote copy of the model.
- `onRestore: (M, A) => F[Unit]`: Effect triggered when the property is set via undo. Usually, it's the same effect as `onSet`. However, sometimes the remote repository directly supports an undo operation, like "undelete".

**Notes**:
- *Currying*: The first four parameters can be specified to obtain an "undoable setter" of a specific property of `M`.
- This approach modifies the local model in-place and sends an async modification to the remote copy. This is done in order to get a snappy UI and not have to wait for a server roundtrip on every change.

After setting a property this way, you can invoke `undo` and `redo` on the `UndoContext` and it will perform the expected operations, including triggering `onRestore` and `onSet`:
```scala
  val undo: Callback
  val redo: Callback
```

## `View` interface

For convenience, you can obtain a `View[A]` on a property of type `A` of the model `M` with the method:
```scala
def undoableView[A](getN: M => A, modN: (A => A) => M => M): View[A]
```

This allows us to pass the `View[A]` into any API that accepts `View` and undo functionality will be transparently added to it. You can specify an effect to be triggered (like `onSet` above) via the `.withOnMod` method of the `View`.

**Notes**:
- No `onRestore`: This approach doesn't let you specify different effects for setting and restoring. 
- *Granularity*: If you `zoom` on the resulting `View[A]`, the undo granularity will still be `A`. This is only an issue when there are concurrent changes (see [Multiplayer](#multiplayer) below). However, the `UndoContext` supports the `zoom` operation. This is what you should you and then call `undoableView` at last possible moment, when you don't plan on zooming in further.

## `Action`

An `Action[M, A]` is just a wrapper of the 4 parameters needed to invoke (`UndoContext[M].set[A]`)[#link].

You then pass an `UndoContext[M]` to its `set` and `mod` methods:
```scala
  def set(undoCtx: UndoSetter[M])(v: A): Callback
  def mod(undoCtx: UndoSetter[M])(f: A => A): Callback
```

This approach allows you to invert the logic and have a clean wrapper of the 4 parameters without the need for an `UndoContext`. This is especially useful for complex operations, like insertions and deletions in lists.

## `Aligner`

### Motivation

Our remote GraphQL APIs define `Input` types to modify values. These are "delta" structures that reflect the structure of the underlying model type, but where all properties are optional and are also `Input` types.

### Solution

An `Aligner` matches a model type `M` with its `Input` equivalent (which we shall call `T`). It also wraps the function `T => F[Unit]` needed to change the value remotely.

> #### Example (part 1, creating an `Aligner`):
> 
> Suppose we are editing a `Target`. That would be the `M` in the description above.
> 
> Suppose we have a method to modify targets in the remote DB is defined as (simplified from actual codebase):
> ``` scala
> def updateTarget(targetId: Target.Id)(input: TargetPropertiesInput): F[Unit]
> ```
> where
> ``` scala
> case class TargetPropertiesInput(
>   name: clue.data.Input[NonEmptyString] = clue.data.Ignore,
>   sidereal: clue.data.Input[SiderealInput] = clue.data.Ignore,
>   nonsidereal: clue.data.Input[NonsiderealInput] = clue.data.Ignore,
>   sourceProfile: clue.data.Input[SourceProfileInput] = clue.data.Ignore,
>   existence: clue.data.Input[Existence] = clue.data.Ignore
> )
> ```
> 
> Assuming we have an `undoCtx: UndoContext[Target]` (and the `targetId: Target.Id` of the target we are editing), we can build an `Aligner[Target, TargetPropertiesInput]`:
> ``` scala
> val targetAligner: Aligner[Target, TargetPropertiesInput] =
>   Aligner(
>     undoCtx,
>     TargetPropertiesInput(),
>     updateTarget(targetId)  // We need to pass a TargetPropertiesInput => F[Unit]
>   )
> ```

Furthermore, an `Aligner` allows `zoom`ing into a property `prop: A` of `M` (via `get`/`mod` functions or a `Lens[M, A]`), while keeping track of how the "delta" `T` type should be modified in order to send a change to `prop` to the remote API. In other words, it aligns local and remote changes to the model.

To achieve this, we must provide a way to drill down from `M` into `A` and from `T` into `S`, where `S` is the `Input` reprsentation of `A`.

> #### Example (part 2, zooming into an `Aligner`):
>
> Suppose we want to edit the target's `name` and that we have:
> ``` scala
> object Target:
>   val name: Lens[Target, NonEmptyString]
>
> object TargetPropertiesInput:
>   val name: Lens[TargetPropertiesInput, clue.data.Input[NonEmptyString]] 
> ```
>
> Here, `A = NonEmptyString` and `S = clue.data.Input[NonEmptyString]`.
>
> Then we can zoom into the `Aligner`:
> ``` scala
> val targetNameAligner: Aligner[NonEmptyString, clue.data.Input[NonEmptyString]] =
>   targetAligner.zoom(Target.name, TargetPropertiesInput.name)
> ```

Finally, we can get `View[A]` out of an `Aligner[A, S]`. This `View[A]` can be passed to input controls that accept `View`s. When the `View` is modified, it will automatically handle pushing the old value into the undo stack and invoking the remote mutation effect.

To build the `View[A]`, we just need a way to turn `A` into its `Input` version: `A => S`.

> #### Example (part 3, getting a `View` out of an `Aligner`):
>
> ``` scala
> val targetNameView: View[NonEmptyString] =
>   targetNameAligner.view(_.assign)
> ```

**Important:** When drilling down, we can turn an `Aligner` into a `View` at any moment and then keep drilling by `zoom`ing in the `View`. We don't want to do this and should turn an `Aligner` into a `View` at the last possible moment. The mutation sent to the DB will be more granular this way.

## Multiplayer

We support external changes to the model `M`.

Executing `set` + `undo` + `redo` will restore the value present right before `undo`. The value may have been modified externally between the `set` and the `undo`, so it may be different than the one specified in the `set`.

The best way to illustrate this is with an example. Suppose we have a property of type `Int` and following sequence of events:
- Original value is `0`.
- User changes value to `1`.
- There's an external modification. Now the value is `2`.
- User undoes. This restores the value to the one before the user edited. The value is now `0`.
- User redoes. The value is now `2`.

This is the approach chosen by Figma, as explained on their [blog post](https://www.figma.com/blog/how-figmas-multiplayer-technology-works/#implementing-undo), and we chose to mimic it.

## Dealing with collections

In order to deal with deletions, insertions and rearrangements in lists and trees we use auxiliary structures. This allows granular control over elements so that remote changes are honored in multiplayer scenarios.

In these auxiliary structures, the (optional) index of an element in the structure is encoded together with the element.

This means that the index of the element can be treated just like another property. The structure will take care of rearranging and recomputing the indices of the rest of the elements if necessary. An index of `None` means that the element is not in the collection. This is the way to delete an element (or undo an insertion).

In order to uniquely identify elements, they need to have a key `K`.

### `KeyedIndexedList[K, A]`

For lists, the auxiliary structure to use is `KeyedIndexedList[K, A]`:
- It can be built from a `List[A]` and a key function `A => K`:
  ```scala
    def fromList[K, A](list: List[A], getKey: A => K): KeyedIndexedList[K, A]
  ```
- The index is an `Option[Int]`, indicating the position of the element in the list.

### `KeyedIndexedTree[K, A]`

First, we define a collection representing a tree as:
```scala
case class Tree[A](children: List[Node[A]])
case class Node[A](value: A, children: List[Node[A]])
```

The auxiliary structure to use is `KeyedIndexedTree[K, A]`:
- It can be built from a `Tree[A]` and a key function `A => K`:
  ```scala
    def fromTree[K: Eq, A](tree: Tree[A], getKey: A => K): KeyedIndexedTree[K, A]
  ```
- The index is represented as:
  ```scala
    case class Index[K](parentKey: Option[K], childPos: NonNegInt)
  ```
  which represent's the node's parent element and the element's position among its siblings.

## Summary (when to use what)

- Create and `UndoContext[M]` by providing:
  - A `View[M]`.
  - A `View[UndoStacks[IO, M]]`. Should be initialized to `UndoStacks.empty[IO, M]`.
- If the model contains collections where you want to have granular inserts, deletes and node repositioning, they should be represented as `KeyedIndexedList`s or `KeyedIndexedTree`s.
  - If you don't care about overwriting remote changes when you undo an operation in a collection, you can have `List`s or other structures.
- Define `Action`s to encapsulate complex operations.
- For dealing with a single level of data, `zoom` into it and build `View`s of the properties by invoking `undoableView`. Pass these `View`s to components that accept `View`s.
- For dealing with an ADT which has a parallel "input" ADT to indicate granular updates to a remote storage, use an `Aligner`.
  - `zoom` into the `Aligner` and build `View`s by invoking `view` just before passing it to components that accept `View`s.
- Invoke `undo` or `redo` on the `UndoContext`.
  - `UndoContext` also has:
    - `isUndoEmpty: Boolean` and `isRedoEmpty: Boolean` properties.
    - `working: Boolean` property, indicating whether a remote invocation is in progress.
