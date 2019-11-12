package prooftree

import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  private val programInput = document.getElementById("programInput").asInstanceOf[dom.html.TextArea]
  private val proofTree = document.getElementById("proofTree").asInstanceOf[dom.html.TextArea]

  @JSExportTopLevel("runProgram")
  def runProgram(): Unit = {
    proofTree.value = ProofTreeDrawer.run(programInput.value)
  }

  def main(args: Array[String]): Unit = {
  }
}