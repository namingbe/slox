import scala.collection.mutable

class Scanner(val source: String) {
  val tokens = mutable.ArrayBuffer[Token]()
  // first char in the lexeme being scanned
  var start = 0
  // char currently considered, start ≤ current
  var current = 0
  // line in the source
  var line = 1

  def scanTokens(): List[Token] = {
    while (current < source.length) {
      start = current
      scanToken()
    }
    tokens.addOne(Token(TokenType.EOF, "", null, line))
    tokens.toList
  }

  def scanToken(): Unit = ???
}
