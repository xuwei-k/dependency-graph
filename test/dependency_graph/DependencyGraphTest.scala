package dependency_graph

import java.util.Arrays

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
      val a1 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.DOT, true)
      val a2 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.DOT, false)
      assert(a1.length < a2.length)
    }
    it("svg") {
      val a1 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.SVG, true)
      println(a1)
      val x = XML.loadString(a1)
      assert((x \ "g" \ "title").text === title)
      val a2 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.SVG, false)
      assert(a1.length < a2.length)
    }
    it("png") {
      val a1 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.PNG, true)
      val a2 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.PNG, false)
      assert(a1.length < a2.length)
    }
    it("gif") {
      val a1 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.GIF, true)
      val a2 = DependencyGraph.withDependencies(play :: Nil, title, GraphType.GIF, false)
      assert(a1.length < a2.length)
    }
  }
}
