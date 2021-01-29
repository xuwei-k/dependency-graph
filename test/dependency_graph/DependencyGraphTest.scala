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
    describe("svg") {
      val redirect = DependencyGraph.svg(play :: Nil, title, LinkType.Redirect, true)
      val none = DependencyGraph.svg(play :: Nil, title, LinkType.None, false)
      val embed = DependencyGraph.svg(play :: Nil, title, LinkType.Embed, false)
      val playURL = "playframework.com"
      val redirectURL = "redirect-project-page"
      it("filterRoot") {
        val x = XML.loadString(redirect)
        assert((x \ "g" \ "title").text === title)
        val redirectRoot = DependencyGraph.svg(play :: Nil, title, LinkType.Redirect, false)
        assert(redirect.length < redirectRoot.length)
      }
      it("redirect") {
        assert(!redirect.contains(playURL))
        assert(redirect.contains(redirectURL))
      }
      it("link none") {
        assert(redirect.length > none.length)
        assert(!none.contains(playURL))
        assert(!none.contains(redirectURL))
      }
      it("link embed") {
        assert(embed.length > none.length)
        assert(embed != redirect)
        assert(embed.contains(playURL))
        assert(!embed.contains(redirectURL))
      }
      it("pom") {
        val pom = DependencyGraph.svg(play :: Nil, title, LinkType.Pom, false)
        assert(pom.contains("https://repo1.maven.org/maven2"))
        assert(none.length < pom.length)
      }
      it("src") {
        val src = DependencyGraph.svg(play :: Nil, title, LinkType.Src, false)
        assert(src.contains("https://java-src.appspot.com/"))
        assert(none.length < src.length)
      }
      it("doc") {
        val doc = DependencyGraph.svg(play :: Nil, title, LinkType.Doc, false)
        assert(doc.contains("https://oss.sonatype.org/service/local/repositories/releases/archive"))
        assert(none.length < doc.length)
      }
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
