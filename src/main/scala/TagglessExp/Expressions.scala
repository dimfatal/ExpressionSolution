package TagglessExp

trait Num[F[_], T] {
  def const(n: Int): F[T]
}

trait Negation[F[_], T] {
  def neg(e: F[T]): F[T]
}

trait Addition[F[_], T] {
  def add(e1: F[T], e2: F[T]): F[T]
}

trait Multiplication[F[_], T] {
  def mul(e1: F[T], e2: F[T]): F[T]
}

trait Division[F[_], T] {
  def div(e1: F[T], e2: F[T]): F[T]
}
