package prooftree

// Expression in CFWAE
trait Expr {
  def toReducedString(numNotReduce: Int): String
}

case class Num(num: Int) extends Expr {
  override def toString: String = num.toString

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None => num.toString
  }
}

case class Add(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{+ $left $right}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None if numNotReduce == 0 =>
      val rs = f"e${Reduced.exprs.size}%01d"
      Reduced.exprs += (this -> rs)
      rs
    case _ =>
      val leftStr = left.toReducedString(numNotReduce - 1)
      val rightStr = right.toReducedString(numNotReduce - 1)
      s"{+ $leftStr $rightStr}"
  }
}

case class Sub(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{- $left $right}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None if numNotReduce == 0 =>
      val rs = f"e${Reduced.exprs.size}%01d"
      Reduced.exprs += (this -> rs)
      rs
    case _ =>
      val leftStr = left.toReducedString(numNotReduce - 1)
      val rightStr = right.toReducedString(numNotReduce - 1)
      s"{- $leftStr $rightStr}"
  }
}

case class Mul(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{* $left $right}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None if numNotReduce == 0 =>
      val rs = f"e${Reduced.exprs.size}%01d"
      Reduced.exprs += (this -> rs)
      rs
    case _ =>
      val leftStr = left.toReducedString(numNotReduce - 1)
      val rightStr = right.toReducedString(numNotReduce - 1)
      s"{* $leftStr $rightStr}"
  }
}

case class With(name: String, value: Expr, body: Expr) extends Expr {
  override def toString: String = s"{with {$name $value} $body}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None if numNotReduce == 0 =>
      val rs = f"e${Reduced.exprs.size}%01d"
      Reduced.exprs += (this -> rs)
      rs
    case _ =>
      val valueStr = value.toReducedString(numNotReduce - 1)
      val bodyStr = body.toReducedString(numNotReduce - 1)
      s"{with {$name $valueStr} $bodyStr}"
  }
}

case class Id(name: String) extends Expr {
  override def toString: String = name

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None => name
  }
}

case class Fun(param: String, body: Expr) extends Expr {
  override def toString: String = s"{fun {$param} $body}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None if numNotReduce == 0 =>
      val rs = f"e${Reduced.exprs.size}%01d"
      Reduced.exprs += (this -> rs)
      rs
    case _ =>
      val bodyStr = body.toReducedString(numNotReduce - 1)
      s"{fun {$param} $bodyStr}"
  }
}

case class App(fun: Expr, arg: Expr) extends Expr {
  override def toString: String = s"{$fun $arg}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None if numNotReduce == 0 =>
      val rs = f"e${Reduced.exprs.size}%01d"
      Reduced.exprs += (this -> rs)
      rs
    case _ =>
      val funStr = fun.toReducedString(numNotReduce - 1)
      val argStr = arg.toReducedString(numNotReduce - 1)
      s"{$funStr $argStr}"
  }
}

case class If0(cond: Expr, tru: Expr, fls: Expr) extends Expr {
  override def toString: String = s"{if0 $cond $tru $fls}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None if numNotReduce == 0 =>
      val rs = f"e${Reduced.exprs.size}%01d"
      Reduced.exprs += (this -> rs)
      rs
    case _ =>
      val condStr = cond.toReducedString(numNotReduce - 1)
      val truStr = tru.toReducedString(numNotReduce - 1)
      val flsStr = fls.toReducedString(numNotReduce - 1)
      s"{if0 $condStr $truStr $flsStr}"
  }
}