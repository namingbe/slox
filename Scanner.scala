import scala.collection.mutable
import TokenType.*

class Scanner(val source: String) {
  private val tokens = mutable.ArrayBuffer[Token]()
  // first char in the lexeme being scanned
  private var start = 0
  // char currently considered, start ≤ current
  private var current = 0
  // line in the source
  private var line = 1

  def scanTokens(): List[Token] = {
    while (current < source.length) {
      start = current
      scanToken()
    }
    tokens.addOne(Token(TokenType.EOF, "", null, line))
    tokens.toList
  }

  private def scanToken(): Unit = {
    val c = advance()
    c match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
    }
  }

  private def advance(): Char = {
    val c = source.charAt(current)
    current += 1
    c
  }

  private def addToken(tokenType: TokenType, literal: Any = null): tokens.type = {
    val text = source.substring(start, current)
    tokens.addOne(Token(tokenType, text, literal, line))
  }
}
