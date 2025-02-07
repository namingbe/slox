import Scanner.*

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
      case '/' => if (matches('/')) {
        while (peek != '\n' && !isAtEnd) advance()
      } else { addToken(SLASH) }
      case ' ' | '\r' | '\t' => ()
      case '\n' => line += 1
      case '"' => string()  // supports multiline strings
      case c if isDigit(c) => number()
      case c if isAlpha(c) => identifier()
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

  private def string(): Unit = {
    while (!isAtEnd && peek != '"') {
      if (peek == '\n')
        line += 1
      advance()
    }
    if (isAtEnd) {
      Lox.error(line, "Unterminated string.")
    }
    advance() // consume the closing quote
    addToken(STRING, source.substring(start + 1, current - 1))
  }

  private def number(): Unit = {
    while (isDigit(peek)) advance()
    if (peek == '.' && isDigit(peekNext)) {
      advance()
      while (isDigit(peek)) advance()
    }
    addToken(NUMBER, source.substring(start, current).toDouble)
  }

  private def identifier(): Unit = {
    while (isAlphanumeric(peek)) advance()
    val text = source.substring(start, current)
    val tokenType = keywords.getOrElse(text, IDENTIFIER)
    addToken(tokenType)
  }

  private def isAtEnd = source.length <= current
  private def peek = if isAtEnd then '\u0000' else source(current)
  private def peekNext = if current + 1 >= source.length then '\u0000' else source(current + 1)
}

object Scanner {
  private val keywords: Map[String, TokenType] = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE,
  )

  private def isDigit(c: Char) = c >= '0' && c <= '9'
  private def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_')
  private def isAlphanumeric(c: Char) = isDigit(c) || isAlpha(c)

}