package TagglessExp

trait EvalProgram[F[_], T] {
  def run: F[T]
}

object EvalProgram {
  object SimpleExpression {
    def dsl[F[_], T](implicit
                     C: Num[F, T],
                     N: Negation[F, T],
                     A: Addition[F, T]) : EvalProgram[F, T] = {
      new EvalProgram[F, T] {
        import C._, N._, A._
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

  object MultiplicationExpression {
    def dsl[F[_], T](implicit
                     C: Num[F, T],
                     N: Negation[F, T],
                     A: Addition[F, T],
                     M: Multiplication[F, T]
                    ): EvalProgram[F, T] =
      new EvalProgram[F, T] {
        import C._, N._, A._, M._
        val run: F[T] =
          add(
            mul(const(2), const(2)),
            mul(const(3), neg(add(const(3), const(7))))
          ) // (2 * 2) + (3 * (- (3 + 7))) = -26
      }
  }

  object DivisionExpression {
    def dsl[F[_], T](implicit
                     C: Num[F, T],
                     N: Negation[F, T],
                     A: Addition[F, T],
                     M: Multiplication[F, T],
                     D: Division[F, T]
                    ): EvalProgram[F, T] =
      new EvalProgram[F, T] {
        import C._, N._, A._, M._, D._

        val run: F[T] =
          div(
            add(
              mul(const(2), const(2)),
              mul(const(3), neg(add(const(3), const(7))))
            ),
            const(2)
          )
        // ( (2 * 2) + (3 * (- (3 + 7))) ) / 2 = -13
      }
  }

  object DivisionByZeroExpression {
    def dsl[F[_], T](implicit
                     C: Num[F, T],
                     N: Negation[F, T],
                     A: Addition[F, T],
                     M: Multiplication[F, T],
                     D: Division[F, T]
                    ): EvalProgram[F, T] =
      new EvalProgram[F, T] {
        import C._, N._, A._, M._, D._

        val run: F[T] =
          div(
            add(
              mul(const(2), const(2)),
              mul(const(3), neg(add(const(3), const(7))))
            ),
            const(0)
          )
        // ( (2 * 2) + (3 * (- (3 + 7))) ) / 0 = -error
      }
  }

  object DivisionNonInteger {
    def dsl[F[_], T](implicit
                     C: Num[F, T],
                     N: Negation[F, T],
                     A: Addition[F, T],
                     M: Multiplication[F, T],
                     D: Division[F, T]
                    ): EvalProgram[F, T] =
      new EvalProgram[F, T] {
        import C._, N._, A._, M._, D._

        val run: F[T] =
          div(
            add(
              mul(const(2), const(2)),
              mul(const(3), neg(add(const(3), const(7))))
            ),
            const(5)
          )
        // ( (2 * 2) + (3 * (- (3 + 7))) ) / 5 = error
      }
  }

  object DivisionByZeroAndNonInteger {
    def dsl[F[_], T](implicit
                     C: Num[F, T],
                     A: Addition[F, T],
                     D: Division[F, T]
                    ): EvalProgram[F, T] =
      new EvalProgram[F, T] {
        import C._, A._, D._

        val run: F[T] =
          add(div(const(5), const(0)), div(const(4), const(3)))      // ((5 / 0) + (4 / 3)) = error, error
      }
  }
}
