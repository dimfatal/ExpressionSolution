package WithSummon.Func

import cats.data._

object ExpressionConsoleEval extends App {

  import WithSummon.Func.EvalInterpreter.Addition._
  import WithSummon.Func.EvalInterpreter.Division._
  import WithSummon.Func.EvalInterpreter.Multiplication._
  import WithSummon.Func.EvalInterpreter.Negation._
  import WithSummon.Func.EvalInterpreter.Num._

  import scala.util.chaining._

  ExpressionProgram.SimpleExpression.dsl[EitherNec[String, *], Int]
    .run
    .tap(println)


}

object ExpressionConsoleView extends App {

  import WithSummon.Func.ViewInterpreter.Addition._
  import WithSummon.Func.ViewInterpreter.Division._
  import WithSummon.Func.ViewInterpreter.Multiplication._
  import WithSummon.Func.ViewInterpreter.Negation._
  import WithSummon.Func.ViewInterpreter.Rub._

  import scala.util.chaining._

  ExpressionProgram.SimpleExpression.dsl[EitherNec[String, *], String]
    .run
    .tap(println)


}
