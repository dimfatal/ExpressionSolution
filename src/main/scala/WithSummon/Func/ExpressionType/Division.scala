package WithSummon.Func.ExpressionType

trait Division[F[_], T] {
  def div(e1: F[T], e2: F[T]): F[T]
}

object Division {

  trait Ops[F[_], T] {
    def typeClassInstance: Division[F, T]
    def self: F[T]
    def / (y: Int)(implicit num: Rub[F, T]) : F[T] = typeClassInstance.div(self, num.const(y))
  }

  object ops {
    implicit def toAllDivOps[F[_], T](target: F[T])(implicit tc: Division[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Division[F, T] = tc
      override def self: F[T] = target
    }

    implicit class RichIntDiv[F[_], T](val value: Int) extends AnyVal {
      def / (n: F[T])(implicit m: Division[F, T], r: Rub[F, T]): F[T] = m.div(r.const(value), n)
    }
  }
}