package WithSummon.Func

import WithSummon.Func.EvalProgram.SimpleExpression

import cats._
import cats.syntax.all._

object Expressions {
  trait Num[F[_], T] {
    def const(n: Int): F[T]
  }

  object Num {
    def const[F[_], T](n: Int)(implicit impl: Num[F, T]): F[T] = impl.const(n)
  }

  trait Negation[F[_], T] {
    def neg(e: F[T]): F[T]
  }

  object Negation {
    def neg[F[_], T](n: F[T])(implicit impl: Negation[F, T]): F[T] = impl.neg(n)
  }

  trait Addition[F[_], T] {
    def add(e1: F[T], e2: F[T]): F[T]
  }

  object Addition {
    def add[F[_], T](n1: F[T], n2: F[T])(implicit impl: Addition[F, T]): F[T] = impl.add(n1, n2)
  }

}

object Eval {
  import WithSummon.Func.Expressions._

  object Num {
    def dsl[F[_]: Applicative]: Num[F, Int] = {
      (n: Int) => n.pure[F]
    }

    implicit def dslIdNum: Num[Id, Int] = dsl
  }

  object Negation {
    def dsl[F[_]: Functor]: Negation[F, Int] =
      (c: F[Int]) => c.map(-_)

    implicit def dslIdNeg: Negation[Id, Int] = dsl
  }

  object Addition {
    def dsl[F[_]: Apply: NonEmptyParallel]: Addition[F, Int] =
      (c1: F[Int], c2: F[Int]) => (c1, c2).parMapN(_ + _)

    implicit def dslIdAdd: Addition[Id, Int] = dsl
  }
}


object EvalProgram {

  trait EvalProgram[F[_], T] {
    def run: F[T]
  }

  import WithSummon.Func.Expressions._
  object SimpleExpression {
    def dsl[F[_]: Num[*[_], T]: Negation[*[_], T]: Addition[*[_], T], T] : EvalProgram[F, T] = {

      new EvalProgram[F, T] {
        import Num._
        import Addition._
        import Negation._
        val run: F[T] =
          add(
            const(2),
            neg(
              add(
                const(3),
                const(7)
              )
            )
          ) // 2 + (- (3 + 7)) = -8
      }
    }
  }
}


object ExpressionConsole extends App {
  import Eval.Addition._
  import Eval.Negation._
  import Eval.Num._

  import scala.util.chaining._

  SimpleExpression.dsl[Id, Int]
    .run
    .tap(println)
}
