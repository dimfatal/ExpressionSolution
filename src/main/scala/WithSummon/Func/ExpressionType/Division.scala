package WithSummon.Func.ExpressionType

trait Division[F[_], T] {
  def div(e1: F[T], e2: F[T]): F[T]
}

object Division {
  def mul[F[_], T](n1: F[T],n2: F[T])(implicit impl: Division[F, T]): F[T] = impl.div(n1, n2)

  trait Ops[F[_], T] {
    def typeClassInstance: Division[F, T]
    def self: F[T]
    def / (y: F[T]): F[T] = typeClassInstance.div(self, y)
    def / (y: Int)(implicit num: Rub[F, T]) : F[T] = typeClassInstance.div(self, num.const1(y))
  }

  object ops {
    implicit def toAllDivOps[F[_], T](target: F[T])(implicit tc: Division[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Division[F, T] = tc
      override def self: F[T] = target
    }
  }
}