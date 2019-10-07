package prooftree

case class Paragraph(lines: List[String]) {
  def width: Int = lines.map(_.length).max

  def height: Int = lines.length

  def normalized(height: Int): Paragraph = Paragraph(List.fill(height - this.height)("") ++ lines)

  override def toString: String = lines.mkString("\n")
}

object Paragraph {
  def apply(line: String): Paragraph = Paragraph(List(line))
}