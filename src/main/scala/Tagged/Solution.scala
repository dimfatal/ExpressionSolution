package Tagged

import Tagged.Expr._

object Expr {
  sealed trait Expr[A]
  case class Add[A](n1: Expr[A], n2: Expr[A]) extends Expr[A]
  case class Mul[A](n1: Expr[A], n2: Expr[A]) extends Expr[A]
  case class Num[A](v: BigInt) extends Expr[A]
}

object Eval {
  def apply(expr: Expr[BigInt]): BigInt = expr match {
    case Add(n1, n2) => Eval(n1) + Eval(n2)
    case Mul(n1, n2) => Eval(n1) * Eval(n2)
    case Num(n) => n
  }
}

object TaggedSolution extends App {
  val testExpr = Expr.Mul[BigInt](
    Expr.Add(
      Expr.Num(2),
      Expr.Num(3)
    ),
    Expr.Mul(
      Expr.Num(4),
      Expr.Num(5)
    )
  )

  println(Eval(testExpr))
}
