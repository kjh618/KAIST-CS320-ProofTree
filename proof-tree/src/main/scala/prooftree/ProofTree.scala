package prooftree

case class ProofTree(premises: List[ProofTree], conclusion: MathExpr) {
  // TODO: Use toReducedString
  def toParagraph: Paragraph = {
    def arrange(paragraphs: List[Paragraph]): Paragraph = {
      val maxHeight = paragraphs.map(_.height).max
      val resLines = paragraphs.map(_.normalized(maxHeight).lines).transpose
      val widths = paragraphs.map(_.width).toVector

      Paragraph(resLines.map(_.foldLeft(("  ", 0))((tup, cur) => {
        val (acc, idx) = tup
        (acc + s"%-${widths(idx)}s  ".format(cur), idx + 1)
      })._1))
    }

    if (premises.isEmpty) {
      Paragraph(conclusion.toString)
    }
    else {
      val premisesParagraph = arrange(premises.map(_.toParagraph))
      val bars = "―" * (premisesParagraph.width max (conclusion.toString.length + 4))
      Paragraph(premisesParagraph.lines :+ bars :+ ("  " + conclusion + "  "))
    }
  }

  override def toString: String = toParagraph.toString
}

object ProofTree {
  def apply(mathExpr: MathExpr): ProofTree = ProofTree(Nil, mathExpr)

  def apply(premise: MathExpr, conclusion: MathExpr): ProofTree = ProofTree(List(ProofTree(premise)), conclusion)
}