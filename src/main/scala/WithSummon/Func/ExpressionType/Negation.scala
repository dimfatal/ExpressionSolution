package WithSummon.Func.ExpressionType

trait Negation[F[_], T] {
  def neg(e: F[T]): F[T]
}

object Negation {
  def neg[F[_], T](n: F[T])(implicit impl: Negation[F, T]): F[T] = impl.neg(n)

  trait Ops[F[_], T] {
    def typeClassInstance: Negation[F, T]
    def self: F[T]
    def unary_- : F[T] = typeClassInstance.neg(self)
  }

  object ops {
    implicit def toAllNegOps[F[_], T](target: F[T])(implicit tc: Negation[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Negation[F, T] = tc
      override def self: F[T] = target
    }
  }
}