package playfair

import scala.annotation.tailrec

class Cipher(key: String) {
  val keySquare: Seq[Char] =
    flatKeySquare(Util.removeDuplicateLetters(key.toUpperCase.filter(('A' to 'Z').contains(_)).replaceAll("J", "I")));

  def flatKeySquare(keyDeDup: Seq[Char]): Seq[Char] = {
    (keyDeDup ++ ('A' to 'Z').filter(!keyDeDup.contains(_))).filter(_ != 'J')
  }

  def findLetterInSquare(letter: Char): (Int, Int) = {
    (keySquare.indexOf(letter) / 5, keySquare.indexOf(letter) % 5)
  }

  def indexToLetter(index: (Int, Int)): Char = {
    keySquare((index._1 * 5) + index._2)
  }

  def encodePair(pair: (Char, Char)): (Char, Char) = {
    (findLetterInSquare(pair._1), findLetterInSquare(pair._2)) match {
      case (l1, l2) if l1._1 == l2._1 => (indexToLetter((l1._1, (l1._2+1)%5)), indexToLetter((l2._1, (l2._2+1)%5))) // same row
      case (l1, l2) if l1._2 == l2._2  => (indexToLetter(((l1._1 + 1)%5, l1._2)), indexToLetter(((l2._1+1)%5, l2._2))) // same column
      case (l1 ,l2) => (indexToLetter((l1._1, l2._2)),indexToLetter((l2._1, l1._2)))
    }
  }

  def decodePair(pair: (Char, Char)): (Char, Char) = {
    (findLetterInSquare(pair._1), findLetterInSquare(pair._2)) match {
      case (l1, l2) if l1._1 == l2._1 => (indexToLetter((l1._1, (l1._2-1)%5)), indexToLetter((l2._1, (l2._2-1)%5))) // same row
      case (l1, l2) if l1._2 == l2._2  => (indexToLetter(((l1._1 - 1)%5, l1._2)), indexToLetter(((l2._1 - 1)%5, l2._2))) // same column
      case (l1 ,l2) => (indexToLetter((l1._1, l2._2)),indexToLetter((l2._1, l1._2)))
    }
  }

  def encode(plaintext: String): String = {
    Cipher.preparePlainText(plaintext).map(encodePair).map({case (x, y) => s"$x$y"}).mkString("")
  }

  def decode(ciphertext: String): String = {
    Cipher.preparePlainText(ciphertext).map(decodePair).map({case (x, y) => s"$x$y"}).mkString("")
  }
}

object Cipher {
  def apply(key: String) = new Cipher(key)

  def preparePlainText(plaintext: String): Seq[(Char, Char)] = {
      recursePlainText(Seq[(Char, Char)](), plaintext.toUpperCase.filter(('A' to 'Z').contains(_)).replaceAll("J", "I"))._1
  }

  def pairLetter(candidatePair: Seq[Char]): ((Char, Char), Int) = {
    if (candidatePair.head == candidatePair(1)) ((candidatePair.head, 'X'), 1) else ((candidatePair.head, candidatePair(1)),2)
  }

  @tailrec
  def recursePlainText(parsed: Seq[(Char, Char)], remaining: String): (Seq[(Char, Char)], String) = {
    remaining.length match {
      case 0 => (parsed, "")
      case 1 => (parsed :+ (remaining(0), 'X'), "")
      case _ => pairLetter(remaining.take(2).toSeq) match {case (a,b) => recursePlainText(parsed :+ a, remaining.drop(b))}
    }
  }
}

