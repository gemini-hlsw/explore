// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

package object utils {

  def abbreviate(s: String, maxLength: Int): String =
    if (s.length > maxLength) s"${s.substring(0, maxLength)}\u2026" else s

  implicit class ListOps[A](val list: List[A]) extends AnyVal {
    def modFirstWhere(find: A => Boolean, mod: A => A): List[A] =
      list.indexWhere(find) match {
        case -1 => list
        case n  => (list.take(n) :+ mod(list(n))) ++ list.drop(n + 1)
      }
  }
}
