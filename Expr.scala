sealed trait Expr

object Expr {
  final case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  final case class Grouping(expression: Expr) extends Expr
  final case class Literal(value: Object) extends Expr
  final case class Unary(operator: Token, right: Expr) extends Expr
}