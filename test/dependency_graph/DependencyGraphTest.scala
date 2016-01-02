package dependency_graph

import org.scalatest.FunSpec

import scala.util.Random
import scala.xml.XML

final class DependencyGraphTest extends FunSpec {
  describe("DependencyGraph") {
    val title = Random.alphanumeric.take(100).mkString
    val play = LibraryDependency(
      "com.typesafe.play",
      "play_2.11",
      "2.4.6"
    )
    it("dot") {
      println(DependencyGraph.dot(play :: Nil, title))
    }
    it("svg") {
      val svg = DependencyGraph.svg(play :: Nil, title)
      println(svg)
      val x = XML.loadString(svg)
      assert((x \ "g" \ "title").text === title)
    }
    it("png") {
      DependencyGraph.png(play :: Nil, title)
    }
    it("gif") {
      DependencyGraph.gif(play :: Nil, title)
    }
  }
}
