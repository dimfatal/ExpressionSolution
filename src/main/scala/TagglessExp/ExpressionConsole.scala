package TagglessExp

import TagglessExp.EvalProgram.SimpleExpression
import cats._
import cats.data._

object ConsoleExp extends App {
  import scala.util.chaining._

  import TagglessExp.Eval._

  SimpleExpression
    .dsl[Id, Int](Num.dsl, Negation.dsl, Addition.dsl)
    .run
    .tap(println)

  EvalProgram.MultiplicationExpression
    .dsl[Id, Int](Num.dsl, Negation.dsl, Addition.dsl, Multiplication.dsl)
    .run
    .tap(println)

  EvalProgram.DivisionExpression
    .dsl[EitherNec[String, *], Int](
      Num.dsl,
      Negation.dsl,
      Addition.dsl,
      Multiplication.dsl,
      Division.dsl
    )
    .run
    .tap(println)

  EvalProgram.DivisionByZeroExpression
    .dsl[EitherNec[String, *], Int](
      Num.dsl,
      Negation.dsl,
      Addition.dsl,
      Multiplication.dsl,
      Division.dsl
    )
    .run
    .tap(println)

  EvalProgram.DivisionNonInteger
    .dsl[EitherNec[String, *], Int](
      Num.dsl,
      Negation.dsl,
      Addition.dsl,
      Multiplication.dsl,
      Division.dsl
    )
    .run
    .tap(println)

  EvalProgram.DivisionByZeroAndNonInteger
    .dsl[EitherNec[String, *], Int](
      Num.dsl,
      Addition.dsl,
      Division.dsl
    )
    .run
    .tap(println)

}
