import TokenType.*

import java.lang

class Interpreter extends Expr.Visitor[Any] {
  def evaluate(expr: Expr): Any = expr.accept(this)

  override def visitLiteralExpr(expr: Expr.Literal): Any = expr.value

  override def visitGroupingExpr(expr: Expr.Grouping): Any =
    evaluate(expr.expression)

  extension (x: Any) def toDouble: lang.Double = lang.Double.valueOf(x.toString)

  override def visitUnaryExpr(expr: Expr.Unary): Any = {
    val right = evaluate(expr.right)
    expr.operator.tokenType match {
      case BANG => lang.Boolean.valueOf(!isTruthy(right))
      case MINUS => -right.toDouble
    }
  }

  private def isTruthy(obj: Any): Boolean = obj match {
    case null => false
    case o: Boolean => o
    case _ => true
  }

  override def visitBinaryExpr(expr: Expr.Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tokenType match
      case MINUS => left.toDouble - right.toDouble
      case SLASH => left.toDouble / right.toDouble
      case STAR => left.toDouble * right.toDouble
      case PLUS => (left, right) match {
        case (l: Double, r: Double) => l + r
        case (l: String, r: String) => l + r
      }
      case GREATER => left.toDouble > right.toDouble
      case GREATER_EQUAL => left.toDouble >= right.toDouble
      case LESS => left.toDouble < right.toDouble
      case LESS_EQUAL => left.toDouble <= right.toDouble
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
