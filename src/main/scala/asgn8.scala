// Define asgn8 object as the container.
object asgn8app {

    // Defining an Environment.
    type Env = List[Binding]

    // Case class for Binding.
    case class Binding(name: String, value: Value)

    // Defining sealed trait ExprC for expressions.
    // Allows program to represent various kinds of syntax nodes
    sealed trait ExprC

    // Special Type of class designed ot mkae immutable data modeling easier.
    case class NumC(n: Double) extends ExprC
    case class StringC(s: String) extends ExprC
    case class IdC(name: String) extends ExprC
    case class AppC(fun: ExprC, args: List[ExprC]) extends ExprC
    case class IfC(test: ExprC, thenExpr: ExprC, elseExpr: ExprC) extends ExprC
    case class LamC(args: List[String], body: ExprC) extends ExprC
    case class SeqC(exprs: List[ExprC]) extends ExprC

    // Defining sealed trait Value for possible values.
    sealed trait Value
    case class NumV(n: Double) extends Value
    case class ClosV(args: List[String], body: ExprC, env: Env) extends Value
    case class BoolV(b: Boolean) extends Value
    case class PrimopV(op: (Value, Value) => Value) extends Value
    case class StringV(s: String) extends Value

    def extendEnv(bindings: List[Binding], env: Env): Env = bindings ++ env

      def lookup(name: String, env: Env): Value = {
    env.find(_.name == name) match {
      case Some(Binding(_, value)) => value
      case None => throw new RuntimeException(s"Unbound variable: $name")
    }}

    // Defining our interp.
    def interp(e: ExprC, env: Env): Value = e match {
    case NumC(n) => NumV(n)
    case StringC(s) => StringV(s)
    case IdC(name) => lookup(name, env)
    case LamC(args, body) => ClosV(args, body, env)
    case SeqC(exprs) =>
      exprs.foldLeft[Value](NumV(0)) { (_, expr) =>
        interp(expr, env)
      }
    case AppC(fun, args) =>
      val fval = interp(fun, env) // Evaluate the function
      val argVals = args.map(arg => interp(arg, env)) // Evaluate arguments
      fval match {
        case PrimopV(op) =>
          if (argVals.length == 2) {
            op(argVals.head, argVals(1))
          } else {
            throw new RuntimeException(
              s"Primitive operations must have exactly 2 arguments, got ${argVals.length}"
            )}
        case ClosV(params, body, closureEnv) =>
          if (params.length == argVals.length) {
            val extendedEnv = extendEnv(params.zip(argVals).map {
              case (param, value) => Binding(param, value)
            }, closureEnv)
            interp(body, extendedEnv)
          } else {
            throw new RuntimeException(
              s"Function called with wrong number of arguments. Expected ${params.length}, got ${argVals.length}"
            )}
        case _ =>
          throw new RuntimeException(s"Tried to call a non-function value: $fval")
      }
    case IfC(test, thenExpr, elseExpr) =>
    interp(test, env) match {
        case BoolV(b) =>
        if (b) interp(thenExpr, env) else interp(elseExpr, env)
        case other =>
        throw new RuntimeException(
            s"If condition must be a boolean, got: $other"
        )
    }}

    val add = PrimopV((v1, v2) => (v1, v2) match {
        case (NumV(a), NumV(b)) => NumV(a + b)
        case _ => throw new RuntimeException("Invalid arguments for addition")
    })

    val subtract = PrimopV((v1, v2) => (v1, v2) match {
        case (NumV(a), NumV(b)) => NumV(a - b)
        case _ => throw new RuntimeException("Invalid arguments for subtraction")
    })

    val multiply = PrimopV((v1, v2) => (v1, v2) match {
        case (NumV(a), NumV(b)) => NumV(a * b)
        case _ => throw new RuntimeException("Invalid arguments for multiplication")
    })

    val divide = PrimopV((v1, v2) => (v1, v2) match {
        case (NumV(a), NumV(b)) if b != 0 => NumV(a / b)
        case (NumV(_), NumV(0)) => throw new RuntimeException("Division by zero")
        case _ => throw new RuntimeException("Invalid arguments for division")
    })
}

// TEST CASES
@main def main(): Unit = {
    import asgn8app._

    val env: Env = List( Binding("x", NumV(10)),
      Binding("+", add),
      Binding("-", subtract),
      Binding("*", multiply),
      Binding("/", divide)
      )

    //our test cases
    println(interp(NumC(42), env))


    println(interp(IdC("x"), env))

    val addExpr = AppC(IdC("+"), List(NumC(3), NumC(5)))
    println(interp(addExpr, env))

    val lamExpr = LamC(List("y"), AppC(IdC("+"), List(IdC("y"), NumC(1))))
    val appExpr = AppC(lamExpr, List(NumC(10)))
    println(interp(appExpr, env))

    val envWithBool = env :+ Binding("isTrue", BoolV(true))
    val ifExpr = IfC(IdC("isTrue"), NumC(1), NumC(0))
    println(interp(ifExpr, envWithBool))

    val seqExpr = SeqC(List(NumC(1), NumC(2), NumC(3)))
    println(interp(seqExpr, env))
  }


// Variables of different types
val byteValue: Byte = 127
val intValue: Int = 42
val floatValue: Float = 3.14f
val stringValue: String = "Hello, Scala!"
val boolValue: Boolean = true
val charValue: Char = 'S'

// Special types
val nullValue: String = null
val unitValue: Unit = println("This returns Unit")
val anyValue: Any = "This is Any"
val anyValValue: AnyVal = 100
val nothingValue: Nothing = throw new Exception("This is Nothing")

// Reference types
val anyRefValue: AnyRef = List(1, 2, 3)

