package TagglessExp

import cats._
import cats.implicits._
import cats.data._

object Eval {
  object Num {
    implicit def dsl[F[_]: Applicative]: Num[F, Int] = {
      (n: Int) => n.pure[F]
    }

    implicit def dslIdNum: Num[Id, Int] = dsl

  }

  object Negation {
    implicit def dsl[F[_]: Functor]: Negation[F, Int] =
      (c: F[Int]) => c.map(-_)

    implicit def dslIdNeg: Negation[Id, Int] = dsl

  }

  object Addition {
    implicit def dsl[F[_]: Apply: NonEmptyParallel]: Addition[F, Int] =
      (c1: F[Int], c2: F[Int]) => (c1, c2).parMapN(_ + _)

    implicit def dslIdAdd: Addition[Id, Int] = dsl
  }

  object Multiplication {
    implicit def dsl[F[_]: Apply: NonEmptyParallel]: Multiplication[F, Int] =
      (c1: F[Int], c2: F[Int]) => (c1, c2).parMapN(_ * _)
  }

  object Division {
    implicit def dsl[
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
  }
}
