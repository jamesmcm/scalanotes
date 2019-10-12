package playfair

import org.scalatest.FunSuite

class MainTest extends FunSuite {
  test("Main.test_dedup") {
    assert(Util.removeDuplicateLetters("teststring") == "tesring")
  }
  test("Main.test_keysquare") {
    assert((Cipher("teststring")).keySquare == "TESRINGABCDFHKLMOPQUVWXYZ".toSeq)
  }
  test("Main.test_keysquare2") {
    assert((Cipher("playfairexample")).keySquare == "PLAYFIREXMBCDGHKNOQSTUVWZ".toSeq)
  }
  test("Main.test_plaintext1") {
    assert(Cipher.preparePlainText("teststring") == Seq[(Char, Char)](('T','E'), ('S','T'), ('S', 'T'), ('R', 'I'), ('N', 'G')))
  }
  test("Main.test_plaintext2") {
    assert(Cipher.preparePlainText("tes") == Seq[(Char, Char)](('T','E'), ('S','X')))
  }
  test("Main.test_plaintext3") {
    assert(Cipher.preparePlainText("tees") == Seq[(Char, Char)](('T','E'), ('E','S')))
  }
  test("Main.test_plaintext4") {
    assert(Cipher.preparePlainText("teees") == Seq[(Char, Char)](('T','E'), ('E','X'), ('E', 'S')))
  }
  test("Main.test_plaintext5") {
    assert(Cipher.preparePlainText("teeee") == Seq[(Char, Char)](('T','E'), ('E','X'), ('E', 'X'), ('E', 'X')))
  }
  test("Main.test_plaintext6") {
    assert(Cipher.preparePlainText("jes") == Seq[(Char, Char)](('I','E'), ('S','X')))
  }
  test("Main.test_ciphertext1") {
    assert(Cipher("playfairexample").encode("Hide the gold in the tree stump") == "BMODZBXDNABEKUDMUIXMMOUVIF")
  }
  test("Main.test_decode") {
    assert(Cipher("playfairexample").decode("BMODZBXDNABEKUDMUIXMMOUVIF") == "HIDETHEGOLDINTHETREXESTUMP")
  }
  test("Main.test_ciphertext2") {
    assert(Cipher("").encode("test") == "UDTU")
  }
  test("Main.test_decode2") {
    assert(Cipher("").decode("UDTU") == "TEST")
  }
  test("Main.test_decode3") {
    assert(Cipher("playfairexample").decode("BMOD") == "HIDE")
  }

}
