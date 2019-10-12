package playfair

object Util {
  def removeDuplicateLetters(x: String): String = {
    x.foldLeft(Seq[Char]())((l: Seq[Char], c: Char) => if (l.contains(c)) l else l :+ c).mkString("")
  }
}
