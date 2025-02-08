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
    val path = outputDir + "/" + baseName + ".scala"
    val writer = new PrintWriter(path, "UTF-8")
    writer.println(s"sealed trait $baseName\n\nobject $baseName {")
    val children = for { (className, fields) <- types } yield {
      s"""  case class $className(
         |    ${fields.split(",").map(_.trim).mkString(",\n    ")}
         |  ) extends $baseName
         |""".stripMargin
    }
    writer.println(children.mkString("\n"))
    writer.println("}")
    writer.close()
  }
}
