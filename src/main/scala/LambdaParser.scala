import scala.util.parsing.combinator.JavaTokenParsers

object LambdaParser extends JavaTokenParsers:
  // Direct name parsers - no need to wrap in Var here
  def termName: Parser[TermVar] = ident ^^ TermVar.apply
  def contName: Parser[ContVar] = ident ^^ ContVar.apply

  // Primary expressions: term variables, parenthesized expressions
  def primary: Parser[Expr] =
    termName ^^ Var.apply |
    "(" ~> expr <~ ")"

  // Continuations: [α] M where α is a continuation variable
  def continuation: Parser[Cont] =
    "[" ~> contName ~ "]" ~ application ^^ { case c ~ _ ~ a => Cont(Var(c), a) }

  // Atoms: primary expressions and continuations
  def atom: Parser[Expr] = continuation | primary

  // Applications: left associative sequence of atoms
  def application: Parser[Expr] = rep1(atom) ^^ (_.reduceLeft(Appl.apply))

  // Lambda abstraction: λx.M
  def lambda: Parser[Lam] =
    ("\\" | "λ") ~> termName ~ "." ~ expr ^^ {
      case p ~ _ ~ b => Lam(p, b)
    }

  // Mu abstraction: μα.M
  def mu: Parser[Mu] =
    ("#" | "μ") ~> contName ~ "." ~ expr ^^ {
      case p ~ _ ~ b => Mu(p, b)
    }

  def expr: Parser[Expr] = lambda | mu | application

  def parseExpr(input: String): Either[String, Expr] =
    parseAll(expr, input) match
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(s"Parse error: $msg")
