// === REPL ===
import scala.io.StdIn.readLine
import scala.collection.mutable
import java.io.{PrintWriter, File}
import scala.io.Source

import LambdaParser.parseExpr
import Evaluator.evalSteps

object REPL extends App:
  val env: mutable.Map[String, Expr] = mutable.Map.empty

  def resolve(expr: Expr, bound: Set[Var] = Set.empty): Expr = expr match
    case Var(x) => if bound.exists(_.name == x) then expr else env.getOrElse(x, expr)
    case Appl(f, a) => Appl(resolve(f, bound), resolve(a, bound))
    case Cont(f, a) => Cont(resolve(f, bound), resolve(a, bound))
    case Lam(p, b) => Lam(p, resolve(b, bound + p))
    case Mu(p, b) => Mu(p, resolve(b, bound + p))

  def pretty(expr: Expr): String = expr match
    case Var(x) => x
    case Lam(p, b) => s"λ${p.name}.${pretty(b)}"
    case Mu(p, b) => s"μ${p.name}.${pretty(b)}"
    case Cont(c, a) => s"[${pretty(c)}] ${pretty(a)}"
    case Appl(f, a) => s"${pretty(f)}" + (a match
      case Var(_) => s" ${pretty(a)}"
      case _ => s" (${pretty(a)})")

  def saveEnv(filename: String): Unit =
    val out = new PrintWriter(File(filename))
    try env.foreach { case (k, v) => out.println(s"let $k = ${pretty(v)}") }
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
      else env.foreach((k, v) => println(s"$k = ${pretty(v)}"))
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
              println(s"$name = ${pretty(resolved)}")
            case Left(err) => println(err)
        case _ => println("Usage: let <name> = <expr>")
    case line =>
      LambdaParser.parseExpr(line) match
        case Right(expr) =>
          val resolved = resolve(expr)
          Evaluator.evalSteps(resolved).zipWithIndex.foreach((e, i) => println(s"[$i] ${pretty(e)}"))
        case Left(err) => println(err)
