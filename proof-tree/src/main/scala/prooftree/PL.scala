package prooftree

object PL {
  // TODO: Handle other cases
  trait CFWAE
  case class Num(num: Int) extends CFWAE {
    override def toString: String = num.toString
  }
  case class Add(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{+ $left $right}"
  }
  case class Sub(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{- $left $right}"
  }
  case class Mul(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{* $left $right}"
  }
  case class With(name: String, value: CFWAE, body: CFWAE) extends CFWAE
  case class Id(name: String) extends CFWAE
  case class Fun(param: String, body: CFWAE) extends CFWAE
  case class App(fun: CFWAE, arg: CFWAE) extends CFWAE
  case class If0(cond: CFWAE, tru: CFWAE, fls: CFWAE) extends CFWAE

  type Env = Map[String, Value]
  type BinOp = (Int, Int) => Int

  // TODO: Handle other cases
  trait Value
  case class NumVal(num: Int) extends Value {
    override def toString: String = num.toString
  }
  case class FunVal(param: String, body: CFWAE, funEnv: Env) extends Value
}
