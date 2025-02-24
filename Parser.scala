import Parser.ParseError
import TokenType.*

import scala.annotation.tailrec
import scala.util.Try

class Parser(tokens: Vector[Token]) {
  def parse(): Option[Expr] =
    Try { Some(expression()) }
      .recover { case e: ParseError => None }
      .get

  private var current = 0

  private def sequence(nextExpr: () => Expr, types: TokenType*): Expr = {
    var expr = nextExpr()
    while (matches(types*)) {
      val operator = previous
      val right = nextExpr()
      expr = Expr.Binary(expr, operator, right)
    }
    expr
  }
  private def expression() =  equality()
  private def equality() = sequence(comparison, BANG_EQUAL, EQUAL_EQUAL)
  private def comparison() = sequence(term, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)
  private def term() = sequence(factor, MINUS, PLUS)
  private def factor() = sequence(unary, SLASH, STAR)
  private def unary(): Expr = {
    if (matches(BANG, MINUS)) {
      val operator = previous
      val right = unary()
      Expr.Unary(operator, right)
    } else {
      primary()
    }
  }
  private def primary(): Expr = {
    if (matches(FALSE)) { return Expr.Literal(java.lang.Boolean(false)) }
    if (matches(TRUE)) { return Expr.Literal(java.lang.Boolean(true)) }
    if (matches(NIL)) { return Expr.Literal(null) }
    if (matches(NUMBER, STRING)) { return Expr.Literal(previous.literal) }
    if (matches(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expected ')' after expression.")
      return Expr.Grouping(expr)
    }
    throw error(peek, "Expected expression")
  }

  @tailrec
  private def matches(types: TokenType*): Boolean = {
    if (types.isEmpty) {
      false
    } else if (check(types.head)) {
      advance()
      true
    } else {
      matches(types.tail*)
    }
  }

  private def check(tokenType: TokenType) = !isAtEnd && peek.tokenType == tokenType
  private def advance() = {
    if (!isAtEnd) { current += 1 }
    previous
  }
  private def isAtEnd = peek.tokenType == EOF
  private def peek = tokens(current)
  private def previous = tokens(current - 1)
  private def consume(tokenType: TokenType, message: String) = {
    if (check(tokenType)) {
      advance()
    } else {
      throw error(peek, message)
    }
  }
  private def error(token: Token, message: String) = {
    Lox.error(token, message)
    ParseError()
  }

  private def synchronize(): Unit = {
    advance()
    while (!isAtEnd) {
      if (previous.tokenType == SEMICOLON) { return }
      peek.tokenType match {
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
        case _ => ()
      }
    }
    advance()
  }
}

object Parser {
  private class ParseError extends RuntimeException
}