package WithSummon.PartialApplied

import cats._
import cats.syntax.all._

object Expression {
  trait Num[F[_], T] {
    def const(n: Int): F[T]
  }

  object Num {
    def apply[F[_]] = new PartiallyApplied[F]

    class PartiallyApplied[F[_]] {
      def apply[T]()(implicit impl: Num[F, T]): Num[F, T] = impl
    }
  }

  trait Negation[F[_], T] {
    def neg(e: F[T]): F[T]
  }

  object Negation {
    def apply[F[_]] = new PartiallyApplied[F]

    class PartiallyApplied[F[_]] {
      def apply[T]()(implicit impl: Negation[F, T]): Negation[F, T] = impl
    }
  }

  trait Addition[F[_], T] {
    def add(e1: F[T], e2: F[T]): F[T]
  }

  object Addition {
    def apply[F[_]] = new PartiallyApplied[F]

    class PartiallyApplied[F[_]] {
      def apply[T]()(implicit impl: Addition[F, T]): Addition[F, T] = impl
    }
  }

}

object Eval {

  import WithSummon.PartialApplied.Expression._
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

  import WithSummon.PartialApplied.Expression._
  object SimpleExpression {
    def dsl[F[_]: Num[*[_], T]: Negation[*[_], T]: Addition[*[_], T], T] : EvalProgram[F, T] = {
      val C = Num[F]()
      val N = Negation[F]()
      val A = Addition[F]()
      new EvalProgram[F, T] {
        import A._, C._, N._
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
  import WithSummon.PartialApplied.EvalProgram.SimpleExpression
  import scala.util.chaining._

  import Eval.Addition._
  import Eval.Negation._
  import Eval.Num._

  SimpleExpression.dsl[Id, Int]
    .run
    .tap(println)
}
