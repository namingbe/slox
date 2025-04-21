import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.io.StdIn

object Lox {
  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: slox [script]")
      System.exit(64)
    }
    if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  private def runFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(String(bytes, Charset.defaultCharset()))
    if (hadError)
      System.exit(65)
  }

  @tailrec
  private def runPrompt(): Unit = {
    print("> ")
    val line = StdIn.readLine()
    // .readLine() yields null when Ctrl-D is typed
    if (line != null) {
      run(line)
      hadError = false
      runPrompt()
    }
  }

  private def run(source: String): Unit = {
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    if (hadError) { return }
    tokens.foreach(println)
    val parser = Parser(tokens)
    val maybeExpression = parser.parse()
    maybeExpression match {
      case Some(expression) => println(AstPrinter().print(expression))
      case None => ()
    }
  }

  private var hadError = false

  def error(line: Int, message: String, where: String = ""): Unit = {
    println(s"[line $line] Error$where: $message")
    hadError = true
  }
  def error(token: Token, message: String): Unit = {
    val position = if token.tokenType == TokenType.EOF then " at end" else s" at '${token.lexeme}'"
    error(token.line, message, position)
  }
}