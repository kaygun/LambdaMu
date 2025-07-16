import scala.io.StdIn.readLine
import scala.collection.mutable
import java.io.{PrintWriter, File}
import scala.io.Source

object REPL extends App:
  val env: mutable.Map[String, Expr] = mutable.Map.empty

  def resolve(expr: Expr, bound: Set[Var] = Set.empty): Expr = expr match
    case Var(x) => if bound.exists(_.name == x) then expr else env.getOrElse(x, expr)
    case p: Pair => p.withParts(resolve(p.head, bound), resolve(p.arg, bound))
    case b: Bind => b.withBody(b.param, resolve(b.body, bound + b.param))

  def saveEnv(filename: String): Unit =
    val out = new PrintWriter(File(filename))
    try env.foreach { case (k, v) => out.println(s"let $k = ${v.pretty}") }
    finally out.close()

  def loadEnv(filename: String): Unit =
    for line <- Source.fromFile(filename).getLines do
      if line.startsWith("let ") then
        line.drop(4).split("=", 2).map(_.trim) match
          case Array(name, exprStr) if name.nonEmpty && exprStr.nonEmpty =>
            LambdaParser.parseExpr(exprStr) match
              case Right(expr) => env(name) = resolve(expr)
              case Left(err) => println(s"Failed to load $name: $err")
          case _ => println(s"Invalid line: $line")

  println("Lambda-Mu Calculus REPL. Type :help for help, :q to quit.")
  Iterator.continually(readLine("λ> ")).takeWhile(_ != null).foreach:
    case ":q" => println("Goodbye!"); sys.exit()
    case ":help" =>
      println("Commands:\n  let <name> = <expr>\n  :env\n  :save <file>\n  :load <file>\n  :help\n  :q")
    case ":env" =>
      if env.isEmpty then println("Empty environment")
      else env.foreach((k, v) => println(s"$k = ${v.pretty}"))
    case line if line.startsWith(":save ") =>
      saveEnv(line.stripPrefix(":save ").trim)
    case line if line.startsWith(":load ") =>
      loadEnv(line.stripPrefix(":load ").trim)
    case line if line.startsWith("let ") =>
      line.drop(4).split("=", 2).map(_.trim) match
        case Array(name, exprStr) if name.nonEmpty && exprStr.nonEmpty =>
          LambdaParser.parseExpr(exprStr) match
            case Right(expr) =>
              val resolved = resolve(expr)
              env(name) = resolved
              println(s"$name = ${resolved.pretty}")
            case Left(err) => println(err)
        case _ => println("Usage: let <name> = <expr>")
    case line =>
      LambdaParser.parseExpr(line) match
        case Right(expr) =>
          val result = Evaluator.evalExpr(resolve(expr))
          println(s"${result.pretty}")
        case Left(err) => println(err)
