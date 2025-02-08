package tool

import java.io.PrintWriter

object GenerateAst {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: generate_ast <output directory>")
      System.exit(64)
    }
    val outputDir = args(0)
    // it's a bit comic with Scala, as output is only a bit longer than this
    defineAst(outputDir, "Expr", List(
      "Binary" -> "left: Expr, operator: Token, right: Expr",
      "Grouping" -> "expression: Expr",
      "Literal" -> "value: Object",
      "Unary" -> "operator: Token, right: Expr",
    ))
  }

  private def defineAst(outputDir: String, baseName: String, types: List[(String, String)]): Unit = {
    val children = types.map { case (className, fields) =>
      s"  final case class $className($fields) extends $baseName"
    }.mkString("\n")
    val complete =
      s"""sealed trait $baseName
         |
         |object $baseName {
         |$children
         |}""".stripMargin
    val writer = new PrintWriter(s"$outputDir/$baseName.scala", "UTF-8")
    writer.print(complete)
    writer.close()
  }
}
