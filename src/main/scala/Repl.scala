// === REPL ===
import scala.io.StdIn.readLine
import scala.collection.mutable
import java.io.{PrintWriter, File}
import scala.io.Source
import scala.util.{Try, Success, Failure}

object REPL extends App:
  private val env: mutable.Map[String, Expr] = mutable.Map.empty

  private def resolve(expr: Expr, bound: Set[Var] = Set.empty): Expr = expr match
    case Var(x) => if bound.exists(_.name == x) then expr else env.getOrElse(x, expr)
    case Lam(param, body) => Lam(param, resolve(body, bound + param))
    case Mu(param, body) => Mu(param, resolve(body, bound + param))
    case Appl(head, arg) => Appl(resolve(head, bound), resolve(arg, bound))
    case Cont(head, arg) => Cont(resolve(head, bound), resolve(arg, bound))

  private def saveEnv(filename: String): Unit =
    Try {
      val out = new PrintWriter(File(filename))
      try env.foreach { case (k, v) => out.println(s"let $k = ${v.pretty}") }
      finally out.close()
    } match
      case Success(_) => println(s"Environment saved to $filename")
      case Failure(e) => println(s"Error saving to $filename: ${e.getMessage}")

  private def loadEnv(filename: String): Unit =
    Try {
      for line <- Source.fromFile(filename).getLines do
        if line.trim.startsWith("let ") then
          line.trim.drop(4).split("=", 2).map(_.trim) match
            case Array(name, exprStr) if name.nonEmpty && exprStr.nonEmpty =>
              LambdaParser.parseExpr(exprStr) match
                case Right(expr) => env(name) = resolve(expr)
                case Left(err) => println(s"Failed to load $name: $err")
            case _ => println(s"Invalid line: $line")
    } match
      case Success(_) => println(s"Environment loaded from $filename")
      case Failure(e) => println(s"Error loading from $filename: ${e.getMessage}")

  private def processCommand(input: String): Boolean = input.trim match
    case ":q" | ":quit" => 
      println("Goodbye!")
      false
    case ":help" | ":h" =>
      println("""Commands:
        |  let <n> = <expr>  - Define a variable
        |  :env                 - Show environment
        |  :clear               - Clear environment
        |  :save <file>         - Save environment to file
        |  :load <file>         - Load environment from file
        |  :help, :h            - Show this help
        |  :quit, :q            - Exit REPL
        |  <expr>               - Evaluate expression""".stripMargin)
      true
    case ":env" =>
      if env.isEmpty then println("Empty environment")
      else env.toSeq.sortBy(_._1).foreach((k, v) => println(s"$k = ${v.pretty}"))
      true
    case ":clear" =>
      env.clear()
      println("Environment cleared")
      true
    case line if line.startsWith(":save ") =>
      saveEnv(line.stripPrefix(":save ").trim)
      true
    case line if line.startsWith(":load ") =>
      loadEnv(line.stripPrefix(":load ").trim)
      true
    case line if line.startsWith("let ") =>
      line.drop(4).split("=", 2).map(_.trim) match
        case Array(name, exprStr) if name.nonEmpty && exprStr.nonEmpty =>
          LambdaParser.parseExpr(exprStr) match
            case Right(expr) =>
              val resolved = resolve(expr)
              env(name) = resolved
              println(s"$name = ${resolved.pretty}")
            case Left(err) => println(err)
        case _ => println("Usage: let <n> = <expr>")
      true
    case line if line.nonEmpty =>
      LambdaParser.parseExpr(line) match
        case Right(expr) =>
          val result = Evaluator.evalExpr(resolve(expr))
          println(s"${result.pretty}")
        case Left(err) => println(err)
      true
    case _ => true

  println("λμ-calculus REPL")
  println("Type :help for commands, :q to quit")
  
  Iterator.continually(readLine("λμ> "))
    .takeWhile(_ != null)
    .map(processCommand)
    .takeWhile(identity)
    .foreach(_ => ())
