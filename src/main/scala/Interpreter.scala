import TokenType.*

class Interpreter extends Expr.Visitor[Boolean | Double | String] {
  def interpret(expr: Expr): Unit = {
    try {
      val value = evaluate(expr)
      println(stringify(value))
    } catch {
      case e: RuntimeError => Lox.runtimeError(e)
    }
  }

  def stringify(obj: Boolean | Double | String): String = obj match {
    case null => "nil"
    case x: Double =>
      val text = x.toString
      if text.endsWith(".0") then text.dropRight(2) else text
    case _ => obj.toString
  }

  def evaluate(expr: Expr): Boolean | Double | String = expr.accept(this)

  override def visitLiteralExpr(expr: Expr.Literal): Boolean | Double | String = expr.value

  override def visitGroupingExpr(expr: Expr.Grouping): Boolean | Double | String =
    evaluate(expr.expression)

  override def visitUnaryExpr(expr: Expr.Unary): Boolean | Double | String = {
    val right = evaluate(expr.right)
    expr.operator.tokenType match {
      case BANG => !isTruthy(right)
      case MINUS =>
        checkNumberOperand(expr.operator, right)
        -right.toString.toDouble
    }
  }

  private def isTruthy(obj: Boolean | Double | String): Boolean = obj match {
    case null => false
    case o: Boolean => o
    case _ => true
  }

  private def checkNumberOperand(operator: Token, operand: Boolean | Double | String): Unit = operand match {
    case _: Double => ()
    case _ => throw new RuntimeError(operator, s"Operand must be a number. ${operand.getClass.getCanonicalName}")
  }

  private def checkNumberOperands(operator: Token, left: Boolean | Double | String, right: Boolean | Double | String): Unit = (left, right) match {
    case (_: Double, _: Double) => ()
    case _ => throw new RuntimeError(operator, "Operand must be a number.")
  }

  override def visitBinaryExpr(expr: Expr.Binary): Boolean | Double | String = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tokenType match
      case MINUS =>
        checkNumberOperands(expr.operator, left, right)
        left.toString.toDouble - right.toString.toDouble
      case SLASH =>
        checkNumberOperands(expr.operator, left, right)
        left.toString.toDouble / right.toString.toDouble
      case STAR =>
        checkNumberOperands(expr.operator, left, right)
        left.toString.toDouble * right.toString.toDouble
      case PLUS => (left, right) match {
        case (l: Double, r: Double) => l + r
        case (l: String, r: String) => l + r
        case _ => throw RuntimeError(expr.operator, "Operands must be two numbers or two strings")
      }
      case GREATER =>
        checkNumberOperands(expr.operator, left, right)
        left.toString.toDouble > right.toString.toDouble
      case GREATER_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.toString.toDouble >= right.toString.toDouble
      case LESS =>
        checkNumberOperands(expr.operator, left, right)
        left.toString.toDouble < right.toString.toDouble
      case LESS_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.toString.toDouble <= right.toString.toDouble
      case BANG_EQUAL => !isEqual(left, right)
      case EQUAL_EQUAL => isEqual(left, right)
  }

  private def isEqual(a: Boolean | Double | String, b: Boolean | Double | String): Boolean = {
    if (a == null && b == null) {
      true
    } else if (a == null) {
      false
    } else { a.equals(b) }
  }
}
