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
    tokens.foreach(println)
  }

  private var hadError = false

  def error(line: Int, message: String, where: String = ""): Unit = {
    println(s"[line $line] Error$where: $message")
    hadError = true
  }
}