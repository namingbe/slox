import TokenType.*

import java.lang

class Interpreter extends Expr.Visitor[Any] {
  def interpret(expr: Expr): Unit = {
    try {
      val value = evaluate(expr)
      println(stringify(value))
    } catch {
      case e: RuntimeError => Lox.runtimeError(e)
    }
  }

  def stringify(obj: Any): String = obj match {
    case null => "nil"
    case x: Double =>
      val text = x.toString
      if text.endsWith(".0") then text.dropRight(2) else text
    case _ => obj.toString
  }

  def evaluate(expr: Expr): Any = expr.accept(this)

  override def visitLiteralExpr(expr: Expr.Literal): Any = expr.value

  override def visitGroupingExpr(expr: Expr.Grouping): Any =
    evaluate(expr.expression)

  extension (x: Any) def toDouble: lang.Double = lang.Double.valueOf(x.toString)

  override def visitUnaryExpr(expr: Expr.Unary): Any = {
    val right = evaluate(expr.right)
    expr.operator.tokenType match {
      case BANG => lang.Boolean.valueOf(!isTruthy(right))
      case MINUS =>
        checkNumberOperand(expr.operator, right)
        -right.toDouble
    }
  }

  private def isTruthy(obj: Any): Boolean = obj match {
    case null => false
    case o: Boolean => o
    case _ => true
  }

  private def checkNumberOperand(operator: Token, operand: Any): Unit = operand match {
    case _ : Double => ()
    case _ => throw new RuntimeError(operator, "Operand must be a number.")
  }

  private def checkNumberOperands(operator: Token, left: Any, right: Any): Unit = (left, right) match {
    case (_: Double, _: Double) => ()
    case _ => throw new RuntimeError(operator, "Operand must be a number.")
  }

  override def visitBinaryExpr(expr: Expr.Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tokenType match
      case MINUS =>
        checkNumberOperands(expr.operator, left, right)
        left.toDouble - right.toDouble
      case SLASH =>
        checkNumberOperands(expr.operator, left, right)
        left.toDouble / right.toDouble
      case STAR =>
        checkNumberOperands(expr.operator, left, right)
        left.toDouble * right.toDouble
      case PLUS => (left, right) match {
        case (l: Double, r: Double) => l + r
        case (l: String, r: String) => l + r
        case _ => RuntimeError(expr.operator, "Operands must be two numbers or two strings")
      }
      case GREATER =>
        checkNumberOperands(expr.operator, left, right)
        left.toDouble > right.toDouble
      case GREATER_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.toDouble >= right.toDouble
      case LESS =>
        checkNumberOperands(expr.operator, left, right)
        left.toDouble < right.toDouble
      case LESS_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.toDouble <= right.toDouble
      case BANG_EQUAL => !isEqual(left, right)
      case EQUAL_EQUAL => isEqual(left, right)
  }

  private def isEqual(a: Any, b: Any): Boolean = {
    if (a == null && b == null) {
      true
    } else if (a == null) {
      false
    } else { a.equals(b) }
  }
}
