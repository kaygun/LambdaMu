// === Parser ===
import scala.util.parsing.combinator.JavaTokenParsers

object LambdaParser extends JavaTokenParsers:
  def variable: Parser[Var] = ident ^^ Var.apply
  
  // Primary expressions: variables, parenthesized expressions
  def primary: Parser[Expr] = 
    variable | 
    "(" ~> expr <~ ")"
  
  // Continuations: [α] M where M is an application or primary
  def continuation: Parser[Cont] = 
    "[" ~> ident ~ "]" ~ application ^^ { case c ~ _ ~ a => Cont(Var(c), a) }
  
  // Atoms: primary expressions and continuations
  def atom: Parser[Expr] = continuation | primary
  
  // Applications: left associative sequence of atoms
  def application: Parser[Expr] = rep1(atom) ^^ (_.reduceLeft(Appl.apply))
  
  // Lambda abstraction: λx.body (right-associative)
  def lambda: Parser[Lam] = 
    ("\\" | "λ") ~> ident ~ "." ~ expr ^^ { case p ~ _ ~ b => Lam(Var(p), b) }
  
  // Mu abstraction: μα.body (right-associative)  
  def mu: Parser[Mu] = 
    ("#" | "μ") ~> ident ~ "." ~ expr ^^ { case p ~ _ ~ b => Mu(Var(p), b) }
  
  // Top-level expression: abstractions or applications
  def expr: Parser[Expr] = lambda | mu | application

  def parseExpr(input: String): Either[String, Expr] =
    parseAll(expr, input) match
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(s"Parse error: $msg")
