sealed trait Expr {
  def accept[R](visitor: Expr.Visitor[R]): R
}

object Expr {
  final case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitBinaryExpr(this)
  }
  final case class Grouping(expression: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitGroupingExpr(this)
  }
  final case class Literal(value: Boolean | Double | String) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitLiteralExpr(this)
  }
  final case class Unary(operator: Token, right: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitUnaryExpr(this)
  }

  trait Visitor[R] {
    def visitBinaryExpr(expr: Expr.Binary): R
    def visitGroupingExpr(expr: Expr.Grouping): R
    def visitLiteralExpr(expr: Expr.Literal): R
    def visitUnaryExpr(expr: Expr.Unary): R
  }
}