package lambdamu

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers

enum Expr:
  case Var(name: String)
  case Lam(param: String, body: Expr)
  case App(fun: Expr, arg: Expr)
  case Freeze(alpha: String, expr: Expr)
  case Mu(alpha: String, expr: Expr)

object LambdaMu:
  private val env = mutable.Map.empty[String, Expr]

  def resolve(expr: Expr): Expr = expr match
    case Expr.Var(n) if env.contains(n) => resolve(env(n))
    case Expr.Lam(x, b)                 => Expr.Lam(x, resolve(b))
    case Expr.App(f, a)                 => Expr.App(resolve(f), resolve(a))
    case Expr.Freeze(a, e)              => Expr.Freeze(a, resolve(e))
    case Expr.Mu(a, u)                  => Expr.Mu(a, resolve(u))
    case other                          => other

  def substU(x: String, e: Expr, expr: Expr): Expr = expr match
    case Expr.Var(y)         => if y == x then e else expr
    case Expr.Lam(y, b)      => if y == x then expr else Expr.Lam(y, substU(x, e, b))
    case Expr.App(f, a)      => Expr.App(substU(x, e, f), substU(x, e, a))
    case Expr.Freeze(a, w)   => Expr.Freeze(a, substU(x, e, w))
    case Expr.Mu(a, u)       => Expr.Mu(a, substU(x, e, u))

  def renameN(beta: String, alpha: String, expr: Expr): Expr = expr match
    case Expr.Lam(x, b)      => Expr.Lam(x, renameN(beta, alpha, b))
    case Expr.App(f, a)      => Expr.App(renameN(beta, alpha, f), renameN(beta, alpha, a))
    case Expr.Freeze(g, w)   => Expr.Freeze(if g == beta then alpha else g, renameN(beta, alpha, w))
    case Expr.Mu(g, u)       => if g == beta then expr else Expr.Mu(g, renameN(beta, alpha, u))
    case _                   => expr

  def appN(beta: String, v: Expr, expr: Expr): Expr = expr match
    case Expr.Lam(x, b)      => Expr.Lam(x, appN(beta, v, b))
    case Expr.App(f, a)      => Expr.App(appN(beta, v, f), appN(beta, v, a))
    case Expr.Freeze(a, w)   => Expr.Freeze(a, if a == beta then Expr.App(appN(beta, v, w), v) else appN(beta, v, w))
    case Expr.Mu(a, u)       => if a == beta then expr else Expr.Mu(a, appN(beta, v, u))
    case _                   => expr

  def reduce0(expr: Expr): (Expr, Boolean) = expr match
    case Expr.Freeze(a1, Expr.Freeze(a2, e2)) if a1 == a2 => (Expr.Freeze(a1, e2), true)
    case Expr.App(Expr.Lam(x, u), v)                      => (substU(x, v, u), true)
    case Expr.App(Expr.Mu(b, u), v)                       => (Expr.Mu(b, appN(b, v, u)), true)
    case Expr.Freeze(a, Expr.Mu(b, u))                    => (renameN(b, a, u), true)
    case Expr.Mu(a, Expr.Freeze(b, e)) if a == b          => (e, true)
    case _                                                => (expr, false)

  @annotation.tailrec
  def reduce(expr: Expr): Expr =
    val (e, changed) = reduce1(expr)
    if changed then reduce(e) else e

  def reduce1(expr: Expr): (Expr, Boolean) =
    val reducedExpr = expr match
      case Expr.Lam(x, b)      => val (rb, c1) = reduce1(b); (Expr.Lam(x, rb), c1)
      case Expr.App(f, a)      => val (rf, c1) = reduce1(f); val (ra, c2) = reduce1(a); (Expr.App(rf, ra), c1 || c2)
      case Expr.Freeze(a, w)   => val (rw, c1) = reduce1(w); (Expr.Freeze(a, rw), c1)
      case Expr.Mu(a, u)       => val (ru, c1) = reduce1(u); (Expr.Mu(a, ru), c1)
      case _                   => (expr, false)
    val (e2, c2) = reduce0(reducedExpr._1)
    (e2, reducedExpr._2 || c2)

  def evalSteps(expr: Expr): List[Expr] =
    val buf = mutable.ListBuffer(expr)
    var curr = expr
    var changed = true
    while changed do
      val (next, c) = reduce1(curr)
      if c then buf += next
      curr = next
      changed = c
    buf.toList

  def pretty(expr: Expr): String = pretty(expr, Map.empty, Set.empty)._1

  private def pretty(expr: Expr, env: Map[String, String], used: Set[String]): (String, Set[String]) = expr match
    case Expr.Var(n) => (env.getOrElse(n, n), used)
    case Expr.Lam(orig, body) =>
      val base = Iterator.from(0).map(i => if i == 0 then orig else s"$orig$i").dropWhile(used.contains).next()
      val (bs, u) = pretty(body, env + (orig -> base), used + base)
      (s"λ$base.$bs", u)
    case Expr.App(f, a) =>
      val (fs, u1) = pretty(f, env, used)
      val (as, u2) = pretty(a, env, u1)
      val left = f match
        case Expr.App(_, _) => s"($fs)"
        case _ => fs
      val right = a match
        case Expr.App(_, _) | Expr.Lam(_, _) | Expr.Mu(_, _) | Expr.Freeze(_, _) => s"($as)"
        case _ => as
      (s"$left $right", u2)
    case Expr.Freeze(alpha, e) =>
      val (es, u) = pretty(e, env, used)
      (s"[$alpha]$es", u)
    case Expr.Mu(alpha, e) =>
      val (es, u) = pretty(e, env, used)
      (s"μ$alpha.$es", u)

  object Parser extends JavaTokenParsers:
    def identifier: Parser[String] = "[a-zA-Z_]\\w*".r
    def lam: Parser[Expr]          = ("\\" | "λ") ~> identifier ~ ("." ~> expr) ^^ { case x ~ e => Expr.Lam(x, e) }
    def mu: Parser[Expr]           = ("mu" | "μ") ~> identifier ~ ("." ~> expr) ^^ { case a ~ e => Expr.Mu(a, e) }
    def freeze: Parser[Expr]       = ("[" ~> identifier <~ "]") ~ expr ^^ { case a ~ e => Expr.Freeze(a, e) }
    def parens: Parser[Expr]       = "(" ~> expr <~ ")"
    def variable: Parser[Expr]     = identifier ^^ Expr.Var
    def term: Parser[Expr]         = lam | mu | freeze | parens | variable
    def app: Parser[Expr] = term ~ rep(term) ^^ {
      case t ~ ts => ts.foldLeft(t)(Expr.App)
    }
    def expr: Parser[Expr]         = app
    def parse(input: String): Expr = parseAll(expr, input).getOrElse(throw new IllegalArgumentException("Parse error"))

  def repl(): Unit =
    println("λμ-Calculus REPL (Scala 3). Type :q to quit.")
    var continue = true

    def handleLine(raw: String): Unit =
      val line = raw.trim
      if line.startsWith("let ") && line.contains("=") then
        val parts = line.drop(4).split("=", 2).map(_.trim)
        if parts.length == 2 then
          val name = parts(0)
          val rhs = parts(1)
          try
            val expr = Parser.parse(rhs)
            env(name) = resolve(expr)
            println(s"Defined $name.")
          catch case ex: Exception => println(s"Definition error: ${ex.getMessage}")
        else println("Error: malformed let statement.")
      else if line.startsWith("load \"") && line.endsWith("\"") then
        val filename = line.stripPrefix("load \"").stripSuffix("\"")
        try
          val source = scala.io.Source.fromFile(filename)
          try source.getLines().filter(_.nonEmpty).foreach { ln =>
            println(s"> $ln")
            handleLine(ln.trim)
          } finally source.close()
        catch case ex: Exception => println(s"Could not load '$filename': ${ex.getMessage}")
      else if line.nonEmpty then
        try
          val expr = resolve(Parser.parse(line))
          evalSteps(expr).zipWithIndex.foreach((e, i) => println(s"[$i] ${pretty(e)}"))
        catch case ex: Exception => println(s"Error: ${ex.getMessage}")

    while continue do
      print("> "); Console.flush()
      scala.io.StdIn.readLine() match
        case null | ":q" => continue = false
        case input        => handleLine(input)
    println("Goodbye.")

@main def runRepl(): Unit = LambdaMu.repl()

