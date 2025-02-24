import TokenType.*

import scala.annotation.tailrec

class Parser(tokens: Vector[Token]) {
  private var current = 0

  private def expression() =  equality()
  private def equality() = {
    var expr = comparison()
    while (matches(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)
    }
    expr
  }
  private def comparison() = {
    var expr = term()
    while (matches(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous
      val right = term()
      expr = Expr.Binary(expr, operator, right)
    }
    expr
  }
  private def term() = {
    var expr = factor()
    while (matches(MINUS, PLUS)) {
      val operator = previous
      val right = factor()
      expr = Expr.Binary(expr, operator, right)
    }
    expr
  }
  private def factor() = {
    var expr: Expr = unary()
    while (matches(SLASH, STAR)) {
      val operator = previous
      val right = unary()
      expr = Expr.Binary(expr, operator, right)
    }
    expr
  }
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
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return Expr.Grouping(expr)
    }
    ???
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
  private def consume(tokenType: TokenType, str: String) = ???
}
