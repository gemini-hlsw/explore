# Undo implementation

## Basic idea

Our undo implementation works over a region of the UI. We will call `M` the base type of the model for the region. For example, in a constraint set editor tile, `M` will be `ConstraintSet`.

To get undo functionality, we start by defining an `UndoContext[M]` for the region. An `UndoContext[M]` wraps:
1. A `View[M]`.
2. A `View[UndoStacks[M]]`, which will hold the undo and redo stacks. The `UndoStacks` for the different regions are global and therefore kept in the `RootModel`.

In order for undo to work when editing a property `prop: A` of `M`, we need:

1. A getter to extract `prop: A` from `M` (`get: M => A`).
2. A modder to modify value `prop: A` in `M` (`mod: (A => A) => M => M`).
3. An effect to modify the value in the remote DB (`onSet: A => F[Unit]`).
4. Optionally, another effect to restore a value (`onRestore: A => F[Unit]`). If not set, then `onSet` (3) can be used, but in the case of undeletions, for example, the remote DB may support this operation directly (More on this later).

(1) and (2) are usually specified with a `Lens[M, A]`.

Note that whenever possible, we modify the local model in-place and send an async modification to the remote DB. This is done in order to get a snappy UI and not have to wait for a server roundtrip on every change.

---

The rest of this document will describe the APIs from highest to lowest level, so that you can get up and running as quickly as possible, and only delve deeper if curious or need to do something non-canonical.

## Aligner

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

## UndoContext


for undo: first thing is to have an UndoContext
[6:29 PM] and then you basically have 2 paths
[6:29 PM] if the structure you are undoing over is simple/flat then you can just call .undoableView on the UndoContext
[6:30 PM] and withOnMod to make remote changes
[6:30 PM] but if the structure is complex it pays to use an Aligner
[6:31 PM] and it’s easier to spread the form over several components
[6:31 PM] you turn it into a View at the last moment by calling .view on the Aligner
[6:32 PM] internally, it calls .undoableView(...).withOnMod on the context
[6:32 PM] in both cases you end up with a View that will perform the remote changes and is undoable/redoable



## Action?





## Insertions and deletions