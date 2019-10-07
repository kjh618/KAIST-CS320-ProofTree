package prooftree

import scala.sys.error
import prooftree.PL._

object ProofTreeDrawer {
  def interp(expr: CFWAE, env: Env): (Value, ProofTree) = {
    def applyBinOp(op: BinOp, left: (Value, ProofTree), right: (Value, ProofTree)): (Value, ProofTree) = {
      val (resLeft, treeLeft) = left
      val (resRight, treeRight) = right
      (resLeft, resRight) match {
        case (NumVal(n1), NumVal(n2)) =>
          val res = NumVal(op(n1, n2))
          (res, ProofTree(List(treeLeft, treeRight), s"$expr ⇒ $res"))
        case (v1, v2) =>
          error(s"not both numbers: $v1, $v2")
      }
    }

    // TODO: Handle other cases
    expr match {
      case Num(n) => {
        val res = NumVal(n)
        (res, ProofTree(Nil, s"$expr ⇒ $res"))
      }
      case Add(l, r) =>
        applyBinOp(_ + _, interp(l, env), interp(r, env))
      case Sub(l, r) =>
        applyBinOp(_ - _, interp(l, env), interp(r, env))
      case Mul(l, r) =>
        applyBinOp(_ * _, interp(l, env), interp(r, env))
    }
  }

  def run(str: String): String = interp(PLParser(str), Map())._2.toString
}
