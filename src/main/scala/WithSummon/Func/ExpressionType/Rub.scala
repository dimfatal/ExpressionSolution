package WithSummon.Func.ExpressionType

trait Rub[F[_], T] {
  def variable(n: Int): F[T]
  def const(n: Int): F[T]
}

object Rub {
  trait Ops[F[_], T] {
    def typeClassInstance: Rub[F, T]
    def rub: F[T]
  }

  object ops {
    implicit def toAllRubOps[F[_], T](target: Int)(implicit tc: Rub[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Rub[F, T] = tc

      override def rub: F[T] = tc.variable(target)
    }
  }
}

trait Usd[F[_], T] {
  def const(n: Int): F[T]
}

object Usd {
  def const[F[_], T](n: Int)(implicit impl: Usd[F, T]): F[T] = impl.const(n)

  trait Ops[F[_], T] {
    def typeClassInstance: Usd[F, T]
    def usd: F[T]
  }

  object ops {
    implicit def toAllUsdOps[F[_], T](target: Int)(implicit tc: Usd[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Usd[F, T] = tc

      override def usd: F[T] = tc.const(target)
    }
  }
}
