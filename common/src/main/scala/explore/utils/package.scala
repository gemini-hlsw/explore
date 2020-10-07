package explore

package object utils {

  def abbreviate(s: String, maxLength: Int): String =
    if (s.length > maxLength) s"${s.substring(0, maxLength)}\u2026" else s

}
