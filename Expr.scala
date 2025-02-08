sealed trait Expr

object Expr {
  case class Binary(left: Expr,
                    operator: Token,
                    right: Expr) extends Expr
}
