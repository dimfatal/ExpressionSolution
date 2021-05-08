package WithSummon.Func.TypeClass

trait Addition[F[_], T] {
  def add(e1: F[T], e2: F[T]): F[T]
}

object Addition {
  def add[F[_], T](n1: F[T], n2: F[T])(implicit impl: Addition[F, T]): F[T] = impl.add(n1, n2)

  trait Ops[F[_], T] {
    def typeClassInstance:Addition[F, T]
    def self: F[T]
    def +(y: F[T]): F[T] = typeClassInstance.add(self, y)
  }

  object ops {
    implicit def toAllAdditionOps[F[_], T](target: F[T])(implicit tc: Addition[F, T]): Ops[F, T] = new Ops[F, T] {
      override def typeClassInstance: Addition[F, T] = tc
      override def self: F[T] = target
    }
  }

}
