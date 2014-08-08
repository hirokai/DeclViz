package visualize

import D3._
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSName, JSExport}
import org.scalajs.dom
import D3.Layout.{GraphNode, GraphLink}

// It's important to specify a type for object when defining a JS object.
// Otherwise this causes exception like the following.
// "Uncaught java.lang.ClassCastException: 1 is not an instance of scala.runtime.Nothing$"
trait LinkData extends GraphLink {
  var value: Double = ???
}

trait NodeData extends GraphNode {
  var group: Int = ???
}

trait GraphData extends js.Object {
  var nodes: js.Array[NodeData] = ???
  var links: js.Array[LinkData] = ???
}

//Nesting @JSExport annotation may cause EmptyScope.enter error.
@JSExport
object D3DrawingExamples extends js.JSApp {

  def main(): Unit = {
    dom.document.querySelector("#svg1").appendChild(TestData.test2.reify)
    dom.document.querySelector("#svg2").appendChild(TestData.test.reify)
  }
}
