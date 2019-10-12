package playfair

object Main extends App {
  println(Cipher(args(0)).encode(args(1)))
}

