package WithSummon.Func

import WithSummon.Func.ExpressionType.{Addition, Division, Multiplication, Negation, Rub}
import cats.data.{EitherNec, NonEmptyChain}
import cats.implicits._
import cats.{Applicative, Apply, Functor, Id, MonadError, NonEmptyParallel}

object EvalInterpreter {
  object Num {
    def dsl[F[_]: Applicative]: Rub[F, Int] = new Rub[F, Int]  {
     // (n: Int) => n.pure[F]

      override def variable(n: Int): F[Int] = n.pure[F]

      override def const(n: Int): F[Int] = n.pure[F]
    }

    implicit def dslIdNum: Rub[Id, Int] = dsl
    implicit def dslEitherNecNum: Rub[EitherNec[String, *], Int] = dsl
  }

  object Negation {
    def dsl[F[_]: Apply: NonEmptyParallel]: Negation[F, Int] =
      (c1: F[Int], c2: F[Int]) => (c1, c2).parMapN(_ - _)

    implicit def dslIdNeg: Negation[Id, Int] = dsl
    implicit def dslEitherNecNeg: Negation[EitherNec[String, *], Int] = dsl
  }

  object Addition {
    def dsl[F[_]: Apply: NonEmptyParallel]: Addition[F, Int] =
      (c1: F[Int], c2: F[Int]) => (c1, c2).parMapN(_ + _)

    implicit def dslIdAdd: Addition[Id, Int] = dsl
    implicit def dslEitherNecAdd: Addition[EitherNec[String, *], Int] = dsl
  }

  object Multiplication {
    def dsl[F[_]: Apply: NonEmptyParallel]: Multiplication[F, Int] =
      (c1: F[Int], c2: F[Int]) => (c1, c2).parMapN(_ * _)

    implicit def dslIdMul: Multiplication[Id, Int] = dsl
    implicit def dslEitherNecMul: Multiplication[EitherNec[String, *], Int] = dsl
  }

  object Division {
    def dsl[
      F[_]: MonadError[*[_], NonEmptyChain[String]]: NonEmptyParallel
    ]: Division[F, Int] =
      (c1: F[Int], c2: F[Int]) => (c1, c2).parTupled.flatMap {
        case (c1, 0) =>
          s"zero division error in expression -> $c1 / 0 "
            .pure[NonEmptyChain]
            .raiseError[F, Int]
        case (c1, c2) =>
          if (c1 % c2 == 0) (c1 / c2).pure[F]
          else
            s"non-integer division error in expression -> $c1 / $c2 "
              .pure[NonEmptyChain]
              .raiseError[F, Int]
      }

      implicit def dslIdDiv: Division[EitherNec[String, *], Int] = dsl
  }


}
