package dependency_graph

import org.scalatest.FunSpec

import scala.xml.XML

final class DependencyGraphTest extends FunSpec {
  describe("DependencyGraph") {
    it("generate") {
      val play = LibraryDependency(
        "com.typesafe.play",
        "play_2.11",
        "2.4.6"
      )
      val svg = DependencyGraph.generate(play :: Nil)
      println(svg)
      XML.loadString(svg)
    }
  }
}
