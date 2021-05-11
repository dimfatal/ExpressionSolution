package WithSummon.Func.ExpressionType

trait Multiplication[F[_], T] {
  def mul(e1: F[T], e2: F[T]): F[T]
}

object Multiplication {

  trait Ops[F[_], T] {
    def typeClassInstance: Multiplication[F, T]
    def self: F[T]
    def *(y: Int)(implicit num: Rub[F, T]): F[T] = typeClassInstance.mul(self, num.const1(y))
  }

  object ops {
    implicit def toAllMulOps[F[_], T](target: F[T])(implicit tc: Multiplication[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Multiplication[F, T] = tc

      override def self: F[T] = target

    }
  }
}