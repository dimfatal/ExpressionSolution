package WithSummon.Func

import WithSummon.Func.ExpressionType.{Addition, Division, Multiplication, Negation, Rub}

object ExpressionProgram {

  trait EvalProgram[F[_], T] {
    def run: F[T]
  }

  object SimpleExpression {

    import Addition.ops._
    import Rub.ops._
    import Multiplication.ops._
    import Division.ops._
    import Negation.ops._

    def dsl[F[_]: Rub[*[_], T] : Negation[*[_], T] :  Addition[*[_], T] : Multiplication[*[_], T] : Division[*[_], T], T] : EvalProgram[F, T] = new EvalProgram[F, T] {
      val run: F[T] = 5.rub * 3 / 5 + (-10.rub)
    }
  }
}