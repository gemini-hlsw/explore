// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.util

import cats.Eq
import cats.data.NonEmptySet
import cats.syntax.all.*
import monocle.Iso

import scala.collection.immutable.SortedSet

final class NonEmptySetWrapper[IdSet, Id: Eq](
  self: IdSet,
  iso:  Iso[IdSet, NonEmptySet[Id]]
) {
  private val selfSet = iso.get(self)

  // expose the NonEmptySet methods
  @inline
  def ++(other: IdSet): IdSet = iso.reverseGet(selfSet ++ iso.get(other))

  @inline
  def --(other: IdSet): SortedSet[Id] = selfSet -- iso.get(other)

  @inline
  def add(id: Id): IdSet = iso.reverseGet(selfSet.add(id))

  @inline
  def head: Id = selfSet.head

  @inline
  def filter(f: Id => Boolean): SortedSet[Id] = selfSet.filter(f)

  @inline
  def contains(id: Id): Boolean = selfSet.contains(id)

  @inline
  def exists(f: Id => Boolean): Boolean = selfSet.exists(f)

  @inline
  def forall(f: Id => Boolean): Boolean = selfSet.forall(f)

  @inline
  def intersect(other: IdSet): SortedSet[Id] =
    selfSet.intersect(iso.get(other))

  @inline
  def subsetOf(other: IdSet): Boolean = toSortedSet.subsetOf(iso.get(other).toSortedSet)

  @inline
  def strictSubsetOf(other: IdSet): Boolean =
    this.selfSet =!= iso.get(other) && subsetOf(other)

  @inline
  def size: Long = selfSet.size

  // Note: length is an Int, while size is a Long. Important in some places in the UI
  @inline
  def length: Int = selfSet.length

  @inline
  def toList: List[Id] = selfSet.toList

  @inline
  def toSortedSet: SortedSet[Id] = selfSet.toSortedSet

  // some helper methods
  @inline
  def remove(other: IdSet): Option[IdSet] = fromSet(this -- other)

  @inline
  def removeSet(other: Set[Id]): Option[IdSet] = fromSet(selfSet.toSortedSet -- other)

  @inline
  def removeUnsafe(other: IdSet): IdSet =
    iso.reverseGet(NonEmptySet.fromSetUnsafe(selfSet -- iso.get(other)))

  @inline
  def removeOne(id: Id): Option[IdSet] = fromSet(selfSet - id)

  @inline
  def filterOpt(f: Id => Boolean): Option[IdSet] = fromSet(selfSet.filter(f))

  @inline
  def intersects(other: IdSet): Boolean = intersect(other).nonEmpty

  @inline
  def single: Option[Id] = if (selfSet.length === 1) selfSet.head.some else none

  private def fromSet(set: SortedSet[Id]): Option[IdSet] =
    NonEmptySet.fromSet(set).map(iso.reverseGet)
}

object NonEmptySetWrapper {
  def apply[IdSet, Id: Eq](
    idSet: IdSet,
    iso:   Iso[IdSet, NonEmptySet[Id]]
  ): NonEmptySetWrapper[IdSet, Id] =
    new NonEmptySetWrapper[IdSet, Id](idSet, iso)
}
