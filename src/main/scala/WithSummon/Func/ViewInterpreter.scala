package WithSummon.Func

import WithSummon.Func.ExpressionType._
import cats.data.{EitherNec, NonEmptyChain}
import cats.implicits._
import cats._

object ViewInterpreter {
  object Rub {
    def dsl[F[_]: Applicative]: Rub[F, String] = new Rub[F, String] {
      override def variable(n: Int): F[String] = s"$n rub".pure[F]

      override def const(n: Int): F[String] = s"$n".pure[F]
    }
      //(n: Int) => s"$n".pure[F]

    implicit def dslIdNum: Rub[Id, String] = dsl
    implicit def dslEitherNecNum: Rub[EitherNec[String, *], String] = dsl
  }


  object Negation {
    def dsl[F[_]: Apply: NonEmptyParallel]: Negation[F, String] =
      (c1: F[String], c2: F[String]) => (c1, c2).parMapN((s1, s2) => s"$s1 - $s2")

    implicit def dslIdNeg: Negation[Id, String] = dsl
    implicit def dslEitherNecNeg: Negation[EitherNec[String, *], String] = dsl
  }

  object Addition {
    def dsl[F[_]: Apply: NonEmptyParallel]: Addition[F, String] =
      (c1: F[String], c2: F[String]) => (c1, c2).parMapN((s1, s2) => s"$s1 + $s2")

    implicit def dslIdAdd: Addition[Id, String] = dsl
    implicit def dslEitherNecAdd: Addition[EitherNec[String, *], String] = dsl
  }

  object Multiplication {
    def dsl[F[_]: Apply: NonEmptyParallel]: Multiplication[F, String] =
      (c1: F[String], c2: F[String]) => (c1, c2).parMapN((s1, s2) => s"$s1 * $s2")

    implicit def dslIdMul: Multiplication[Id, String] = dsl
    implicit def dslEitherNecMul: Multiplication[EitherNec[String, *], String] = dsl
  }

  object Division {
    def dsl[
      F[_]: MonadError[*[_], NonEmptyChain[String]]: NonEmptyParallel
    ]: Division[F, String] =
      (c1: F[String], c2: F[String]) => (c1, c2).parMapN((s1, s2) => s"$s1 / $s2")

    implicit def dslIdDiv: Division[EitherNec[String, *], String] = dsl
  }
}
