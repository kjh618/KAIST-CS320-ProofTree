package prooftree

case class ProofTree(premises: List[ProofTree], conclusion: String) {
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
      Paragraph(List(conclusion))
    }
    else {
      val premisesParagraph = arrange(premises.map(_.toParagraph))
      val bars = "―" * (premisesParagraph.width max (conclusion.length + 2))
      Paragraph(premisesParagraph.lines :+ bars :+ ("  " + conclusion))
    }
  }

  override def toString: String = toParagraph.toString
}