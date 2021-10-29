// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptySet
import cats.syntax.all._
import monocle.Iso

import scala.collection.immutable.SortedSet

final class NonEmptySetWrapper[IdSet, Id](self: IdSet, iso: Iso[IdSet, NonEmptySet[Id]]) {
  private val selfSet = iso.get(self)

  // expose the NonEmptySet methods
  def ++(other: IdSet): IdSet = iso.reverseGet(selfSet ++ iso.get(other))

  def --(other: IdSet): SortedSet[Id] = selfSet -- iso.get(other)

  def add(id: Id): IdSet = iso.reverseGet(selfSet.add(id))

  def filter(f: Id => Boolean): SortedSet[Id] = selfSet.filter(f)

  def contains(id: Id): Boolean = selfSet.contains(id)

  def exists(f: Id => Boolean): Boolean = selfSet.exists(f)

  def forall(f: Id => Boolean): Boolean = selfSet.forall(f)

  def intersect(other: IdSet): SortedSet[Id] =
    selfSet.intersect(iso.get(other))

  def size: Long = selfSet.size

  def toList: List[Id] = selfSet.toList

  // some helper methods
  def remove(other: IdSet): Option[IdSet] = fromSet(this -- other)

  def removeUnsafe(other: IdSet): IdSet =
    iso.reverseGet(NonEmptySet.fromSetUnsafe(selfSet -- iso.get(other)))

  def removeOne(id: Id): Option[IdSet] = fromSet(selfSet - id)

  def filterOpt(f: Id => Boolean): Option[IdSet] = fromSet(selfSet.filter(f))

  def hasIntersect(other: IdSet): Boolean = intersect(other).nonEmpty

  private def fromSet(set: SortedSet[Id]): Option[IdSet] =
    NonEmptySet.fromSet(set).map(iso.reverseGet)
}

object NonEmptySetWrapper {
  def apply[IdSet, Id](
    idSet: IdSet,
    iso:   Iso[IdSet, NonEmptySet[Id]]
  ): NonEmptySetWrapper[IdSet, Id] =
    new NonEmptySetWrapper[IdSet, Id](idSet, iso)
}
