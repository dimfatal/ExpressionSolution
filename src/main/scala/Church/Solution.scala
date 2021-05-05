package Church


object Solution {
  type Expr[A] =
    A =>
      ((A, A) => A) =>
      ((A, A) => A) =>
      (BigInt => A) =>
      A

  trait ExprC {
    def apply[A](add: (A, A) => A, mul : (A, A) => A, num: BigInt => A) : A
  }

  object ExprC1 {
    def Add[A](n1: Expr[A], n2: Expr[A]): Expr[A] = ???
    def Mul[A](n1: Expr[A], n2: Expr[A]): Expr[A] = ???
    def Num[A](n: BigInt): Expr[A] = ???
  }

}
