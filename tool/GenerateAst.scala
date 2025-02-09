package tool

import java.io.PrintWriter

object GenerateAst {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: generate_ast <output directory>")
      System.exit(64)
    }
    val outputDir = args(0)
    defineAst(outputDir, "Expr", List(
      "Binary" -> "left: Expr, operator: Token, right: Expr",
      "Grouping" -> "expression: Expr",
      "Literal" -> "value: Object",
      "Unary" -> "operator: Token, right: Expr",
    ))
  }

  private def defineAst(outputDir: String, baseName: String, types: List[(String, String)]): Unit = {
    val children = types.map { case (className, fields) =>
      s"""  final case class $className($fields) extends $baseName {
         |    override def accept[R](visitor: Visitor[R]): R = visitor.visit${className}Expr(this)
         |  }""".stripMargin
    }.mkString("\n")
    val visitor = types.map { case (className, fields) =>
      s"def visit${className}Expr(expr: Expr.$className): R"
    }.mkString("\n    ")
    val complete =
      s"""sealed trait $baseName {
         |  def accept[R](visitor: Expr.Visitor[R]): R
         |}
         |
         |object $baseName {
         |$children
         |
         |  trait Visitor[R] {
         |    $visitor
         |  }
         |}""".stripMargin
    val writer = new PrintWriter(s"$outputDir/$baseName.scala", "UTF-8")
    writer.print(complete)
    writer.close()
  }
}
