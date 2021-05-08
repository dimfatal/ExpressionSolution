package WithSummon.Func.TypeClass

trait Rub[F[_], T] {
  def const(n: Int): F[T]
}

object Rub {
  def const[F[_], T](n: Int)(implicit impl: Rub[F, T]): F[T] = impl.const(n)

  trait Ops[F[_], T] {
    def typeClassInstance: Rub[F, T]
    def rub: F[T]
  }

  object ops {
    implicit def toAllNumOps[F[_], T](target: Int)(implicit tc: Rub[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Rub[F, T] = tc

      override def rub: F[T] = tc.const(target)
    }
  }
}