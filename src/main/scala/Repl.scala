import scala.io.StdIn.readLine
import scala.collection.mutable
import java.io.{PrintWriter, File}
import scala.io.Source
import scala.util.{Try, Success, Failure}

object REPL extends App:
  private val environment: mutable.Map[TermVar, Expr] = mutable.Map.empty
  private var normalizeResults: Boolean = true

  // Helper for safe file operations
  private def withFile[T](filename: String, operation: String)(action: => T): Unit =
    Try(action) match
      case Success(_) => println(s"$operation successful: $filename")
      case Failure(e) => println(s"Error during $operation of $filename: ${e.getMessage}")

  // Resolve variables in expressions
  private def resolve(expr: Expr, bound: Set[Name] = Set.empty): Expr = expr match
    case Var(x) =>
      if bound.contains(x) then Var(x)
      else x match
        case tv: TermVar => environment.getOrElse(tv, Var(tv))
        case cv: ContVar => Var(cv)
    case Lam(param, body) => Lam(param, resolve(body, bound + param))
    case Mu(param, body) => Mu(param, resolve(body, bound + param))
    case Appl(head, arg) => Appl(resolve(head, bound), resolve(arg, bound))
    case Cont(head, arg) => Cont(resolve(head, bound), resolve(arg, bound))

  // File operations
  private def saveEnv(filename: String): Unit = withFile(filename, "Environment save") {
    val out = new PrintWriter(File(filename))
    try environment.foreach { case (k, v) => out.println(s"let ${k.name} = ${v.pretty}") }
    finally out.close()
  }

  private def loadEnv(filename: String): Unit = withFile(filename, "Environment load") {
    for line <- Source.fromFile(filename).getLines do
      if line.trim.startsWith("let ") then
        parseLet(line.trim.drop(4)) match
          case Some((name, expr)) => environment(TermVar(name)) = resolve(expr)
          case None => println(s"Invalid line: $line")
  }

  // Parse let assignments (used by both loadEnv and processCommand)
  private def parseLet(letContent: String): Option[(String, Expr)] =
    letContent.split("=", 2).map(_.trim) match
      case Array(name, exprStr) if name.nonEmpty && exprStr.nonEmpty =>
        LambdaParser.parseExpr(exprStr).toOption.map(name -> _)
      case _ => None

  // Expression evaluation with configurable normalization
  private def evaluateExpression(expr: Expr): Expr =
    val resolved = resolve(expr)
    val evaluated = resolved.eval()
    if normalizeResults then evaluated.normalize() else evaluated

  // Parse and evaluate expression helper
  private def parseAndEvaluate(exprStr: String): Unit =
    LambdaParser.parseExpr(exprStr) match
      case Right(expr) => println(evaluateExpression(expr).pretty)
      case Left(err) => println(err)

  // Show single step reduction
  private def showStep(exprStr: String): Unit =
    LambdaParser.parseExpr(exprStr) match
      case Right(expr) =>
        val resolved = resolve(expr)
        val stepped = resolved.step
        if stepped.alphaEq(resolved) then
          println(s"${resolved.pretty} (already in normal form)")
        else
          println(s"${resolved.pretty} → ${stepped.pretty}")
      case Left(err) => println(err)

  // Show step-by-step trace
  private def showTrace(exprStr: String): Unit =
    LambdaParser.parseExpr(exprStr) match
      case Right(expr) =>
        val resolved = resolve(expr)
        traceReduction(resolved)
      case Left(err) => println(err)

  private def traceReduction(expr: Expr, maxSteps: Int = 400): Unit =
    var current = expr
    var stepCount = 0
    
    println(s"$stepCount: ${current.pretty}")
    
    while stepCount < maxSteps do
      val next = current.step
      if next.alphaEq(current) then
        // No more single steps possible, try normalization if enabled
        if normalizeResults then
          val normalized = current.normalize()
          if !normalized.alphaEq(current) then
            println(s"[$stepCount]: ${normalized.pretty}")
          else
            println("(already in normal form)")
        else
          println("(no more single steps available)")
        return
      else
        stepCount += 1
        current = next
        println(s"[$stepCount]: ${current.pretty}")
    
    println(s"Stopped after $maxSteps steps (may not be in normal form)")
    if normalizeResults then
      println("Attempting full normalization...")
      val normalized = current.normalize()
      println(s"Final (normalized): ${normalized.pretty}")

  // Alpha equivalence checking
  private def checkAlphaEquivalence(line: String): Unit =
    val parts = line.split("==", 2)
    if parts.length != 2 then
      println("Usage: <expr1> == <expr2>")
    else
      val expr1Str = parts(0).trim
      val expr2Str = parts(1).trim
      
      (LambdaParser.parseExpr(expr1Str), LambdaParser.parseExpr(expr2Str)) match
        case (Right(expr1), Right(expr2)) =>
          val resolved1 = resolve(expr1).normalize()
          val resolved2 = resolve(expr2).normalize()
          val areAlphaEq = resolved1.alphaEq(resolved2)
          
          println(s"${resolved1.pretty} == ${resolved2.pretty}")
          println(areAlphaEq)
            
        case (Left(err1), _) => println(s"Parse error in first expression: $err1")
        case (_, Left(err2)) => println(s"Parse error in second expression: $err2")

  // Command processors organized by category
  private object Commands:
    def help(): Unit =
      println("""Commands:
        |  let <n> = <expr>     - Define a variable
        |  :env                 - Show environment
        |  :clear               - Clear environment
        |  :save <file>         - Save environment to file
        |  :load <file>         - Load environment from file
        |  :normalize on|off    - Toggle normalization (default: on)
        |  :step <expr>         - Show single step reduction
        |  :trace <expr>        - Show step-by-step evaluation
        |  :help, :h            - Show this help
        |  :quit, :q            - Exit REPL
        |  <expr>               - Evaluate expression""".stripMargin)

    def showEnv(): Unit =
      if environment.isEmpty then println("Empty environment")
      else environment.toSeq.sortBy(_._1.name).foreach((k, v) => println(s"${k.name} = ${v.pretty}"))

    def clearEnv(): Unit =
      environment.clear()
      println("Environment cleared")

    def setNormalize(setting: String): Unit =
      setting.toLowerCase match
        case "on" | "true" => 
          normalizeResults = true
          println("Normalization enabled - expressions will be fully reduced")
        case "off" | "false" => 
          normalizeResults = false
          println("Normalization disabled - expressions will use lazy evaluation only")
        case _ => println("Usage: :normalize on|off")

    def defineLet(letContent: String): Unit =
      parseLet(letContent) match
        case Some((name, expr)) =>
          val resolved = resolve(expr)
          environment(TermVar(name)) = resolved
          println(s"$name = ${resolved.pretty}")
        case None => println("Usage: let <n> = <expr>")

  // Main command processor
  private def processCommand(input: String): Boolean = input.trim match
    case ":q" | ":quit" => 
      println("Goodbye!")
      false
      
    case ":help" | ":h" => Commands.help(); true
    case ":env" => Commands.showEnv(); true
    case ":clear" => Commands.clearEnv(); true
    
    case line if line.startsWith(":normalize ") =>
      Commands.setNormalize(line.stripPrefix(":normalize ").trim)
      true
      
    case line if line.startsWith(":step ") =>
      showStep(line.stripPrefix(":step ").trim)
      true
      
    case line if line.startsWith(":trace ") =>
      showTrace(line.stripPrefix(":trace ").trim)
      true
      
    case line if line.startsWith(":save ") =>
      saveEnv(line.stripPrefix(":save ").trim)
      true
      
    case line if line.startsWith(":load ") =>
      loadEnv(line.stripPrefix(":load ").trim)
      true
      
    case line if line.contains("==") =>
      checkAlphaEquivalence(line)
      true
      
    case line if line.startsWith("let ") =>
      Commands.defineLet(line.drop(4))
      true
      
    case line if line.nonEmpty =>
      parseAndEvaluate(line)
      true
      
    case _ => true

  // Main REPL loop
  println("λμ-calculus REPL")
  println("Type :help for commands, :q to quit")
  println(s"Normalization: ${if normalizeResults then "enabled" else "disabled"}")

  Iterator.continually(readLine("λμ> "))
    .takeWhile(_ != null)
    .map(processCommand)
    .takeWhile(identity)
    .foreach(_ => ())
