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
    while (!isAtEnd) {
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
      case '!' => addToken(if matches('=') then BANG_EQUAL else BANG)
      case '=' => addToken(if matches('=') then EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if matches('=') then LESS_EQUAL else LESS)
      case '>' => addToken(if matches('=') then GREATER_EQUAL else GREATER)
      case _ => Lox.error(line, "Unexpected character.")
    }
  }

  private def advance(): Char = {
    val c = source(current)
    current += 1
    c
  }

  private def addToken(tokenType: TokenType, literal: Any = null): tokens.type = {
    val text = source.substring(start, current)
    tokens.addOne(Token(tokenType, text, literal, line))
  }

  private def matches(expected: Char): Boolean = {
    if (!isAtEnd && source(current) == expected) {
      current += 1
      true
    } else false
  }

  private def isAtEnd = current >= source.length
}
