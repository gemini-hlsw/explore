// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*
import explore.data.KeyedIndexedList

trait ArbKeyedIndexedList {
  implicit def keyedIndexedListArb[K, A](implicit
    arbA:     Arbitrary[A],
    arbKeyFn: Arbitrary[A => K]
  ): Arbitrary[KeyedIndexedList[K, A]] =
    Arbitrary[KeyedIndexedList[K, A]] {
      for {
        list   <- arbitrary[List[A]]
        getKey <- arbitrary[A => K]
      } yield KeyedIndexedList.fromList(list, getKey)
    }

  implicit def keyedIndexedListCogen[K, A](implicit
    cogenA: Cogen[A]
  ): Cogen[KeyedIndexedList[K, A]] =
    Cogen[List[A]].contramap(_.toList)
}

object ArbKeyedIndexedList extends ArbKeyedIndexedList
