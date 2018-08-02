
object example extends App {

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String  = e match {
    case Number(n) => n.toString()
    case Sum(e1, e2) => show(e1) + "+" + show(e2)
    case Prod(e1, e2) =>
      { e1 match {
        case Sum(e3, e4) => "(" + show(Sum(e3, e4)) + ")"
        case _ => show(e1) }
      } + "*" +
        { e2 match {
          case Sum(e3, e4) => "(" + show(Sum(e3,e4)) + ")"
          case _ => show(e2) }
        }
    case Var(x) => x
  }

  println(eval(Sum(Number(1),Number(2))))

  println(show(Sum(Number(1),Number(2))))

  println(show(Sum(Prod(Number(2), Var("x")), Var("y"))))

  println(show(Prod(Sum(Number(2), Var("x")), Var("y"))))

}
