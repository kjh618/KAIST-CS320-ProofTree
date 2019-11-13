package prooftree

import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  private val programInput = document.getElementById("programInput").asInstanceOf[dom.html.TextArea]
  private val output = document.getElementById("output").asInstanceOf[dom.html.Paragraph]

  @JSExportTopLevel("runProgram")
  def runProgram(): Unit = {
    output.textContent = try ProofTreeDrawer.run(programInput.value) catch {
      case e: RuntimeException => e.getMessage
      case e: Throwable => s"unknown error: $e"
    }
  }

  def main(args: Array[String]): Unit = {
  }
}