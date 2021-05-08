package WithSummon.Func.ExpressionType

trait Negation[F[_], T] {
  def neg(e1: F[T], e2: F[T]): F[T]
}

object Negation {
  def neg[F[_], T](n1: F[T], n2: F[T])(implicit impl: Negation[F, T]): F[T] = impl.neg(n1, n2)

  trait Ops[F[_], T] {
    def typeClassInstance: Negation[F, T]
    def self: F[T]
   // def unary_- : F[T] = typeClassInstance.neg(self)
    def -(y: F[T]): F[T] = typeClassInstance.neg(self, y)
  }

  object ops {
    implicit def toAllNegOps[F[_], T](target: F[T])(implicit tc: Negation[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Negation[F, T] = tc
      override def self: F[T] = target
    }
  }
}