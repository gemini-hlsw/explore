// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package japgolly.webapputil.general

import cats.Eq
import cats.syntax.option.*
import scala.collection.mutable

final case class Version(major: Version.Major, minor: Version.Minor) {
  override def toString = verStr
  def verNum            = s"${major.value}.${minor.value}"
  def verStr            = "v" + verNum
}

object Version {

  def fromInts(major: Int, minor: Int): Version =
    Version(Major(major), Minor(minor))

  final case class Major(value: Int) {
    assert(value >= 1)
  }

  final case class Minor(value: Int) {
    assert(value >= 0)
  }

  implicit val univEqMajor: Eq[Major] = Eq.by(_.value)
  implicit val univEqMinor: Eq[Minor] = Eq.by(_.value)
  implicit val univEq: Eq[Version]    = Eq.by(v => (v.major, v.minor))

  implicit val ordering: Ordering[Version] =
    new Ordering[Version] {
      override def compare(x: Version, y: Version): Int = {
        val i = x.major.value - y.major.value
        if (i != 0)
          i
        else
          x.minor.value - y.minor.value
      }
    }

  private val memoV1: mutable.Map[Int, Version] = mutable.Map.empty
  def v1(minorVer: Int): Version                =
    memoV1
      .updateWith(minorVer):
        case None  => Version.fromInts(1, minorVer).some
        case other => other
      .get

  private val memoV2: mutable.Map[Int, Version] = mutable.Map.empty
  def v2(minorVer: Int): Version                =
    memoV2
      .updateWith(minorVer):
        case None  => Version.fromInts(2, minorVer).some
        case other => other
      .get
}
