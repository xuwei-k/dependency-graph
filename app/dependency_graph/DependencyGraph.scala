package dependency_graph

import java.io.{File, ByteArrayOutputStream, PrintStream}

import sbt.IO
import sbt.Path._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.sys.process.ProcessLogger
import scala.util.control.NonFatal
import scala.xml.{Elem, XML}

object DependencyGraph {
  // https://github.com/jrudolph/sbt-dependency-graph/issues/84
  val pluginSbtContents =
    """addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")"""

  val logger = ProcessLogger(println(_))

  private def dependencyDotHeader(title: String) = {
    s"""dependencyDotHeader in Compile := \"\"\"digraph "$title" {
      graph[rankdir="LR"]
      node [
        shape="record"
      ]
      edge [
        arrowtail="none"
      ]\"\"\""""
  }

  private[this] val findURLMethod = """def findURL(g: String, a: String, v: String): String = {
  val u = s"https://repo1.maven.org/maven2/${g.replace('.', '/')}/${a}/${v}/${a}-${v}.pom"
  try{
    val pom = scala.xml.XML.load(url(u))
    List(
      pom \ "url",
      pom \ "scm" \ "url",
      pom \ "organization" \ "url"
    ).foldLeft(Option.empty[String]){
      case (None, elem) =>
        val url = elem.text
        if(url.startsWith("http")){
          Some(url)
        } else {
          None
        }
      case (a @ Some(_), _) =>
        a
    }.getOrElse(u)
  }catch{
    case scala.util.control.NonFatal(_) =>
      u
  }
}"""

  private def baseSettings(node: String) = """

Seq(Compile, Test, Runtime, Provided, Optional).flatMap{ c =>
  inConfig(c){
    dependencyDot := {
      val nodes = moduleGraph.value.nodes.par.map{ n =>
        val fullId = n.id.organisation + "/" + n.id.name + "/" + n.id.version
        val g = n.id.organisation
        val a = n.id.name
        val v = n.id.version
        "  \"" + n.id.idString + "\"" +
        "[label=<" + n.id.organisation + "<BR/><B>" + n.id.name + "</B><BR/>" + n.id.version + ">" + """ + node + """ +
        ", target=\"_blank\"" +
        ", tooltip=\"" + fullId + "\"" +
        "]"
      }.mkString("\n")

      val edges = moduleGraph.value.edges.map{ e =>
        "  \"" + e._1.idString + "\" -> \"" + e._2.idString + "\""
      }.mkString("\n")

      val dot = List(dependencyDotHeader.value, nodes, edges).mkString("", "\n\n", "\n}")
      sbt.IO.write(dependencyDotFile.value, dot)
      dependencyDotFile.value
    }
  }
}"""

  private val redirect = baseSettings(
    """ ", href=\"https://dependency-graph.herokuapp.com/" + fullId + "/redirect-project-page\"" """
  )

  private val embed = findURLMethod + "\n\n" + baseSettings(
    """ ", href=\"" + findURL(g, a, v) + "\"" """
  )

  private val pom = baseSettings(
    """ ", href=\"" + s"https://repo1.maven.org/maven2/${g.replace('.', '/')}/${a}/${v}/${a}-${v}.pom" + "\"" """
  )

  private val src = baseSettings(
    """ ", href=\"" + s"https://java-src.appspot.com/${g}/${a}/${v}" + "\"" """
  )

  private val doc = baseSettings(
    """ ", href=\"" + s"https://oss.sonatype.org/service/local/repositories/releases/archive/${g.replace('.', '/')}/$a/$v/$a-$v-javadoc.jar/!/index.html" + "\"" """
  )

  private val noLink = baseSettings("\"\"")

  private[this] def withTempDirAndDotFile[A](dependencies: Seq[LibraryDependency], title: String, filterRoot: Boolean = true)(f: (File, File) => A): A = {
    val buildDotSbt = defaultBuildDotSbt(title)
    generate(buildDotSbt + "\n\n" + dependencies.mkString("\n\n"), filterRoot)(f)
  }

  def defaultBuildDotSbt(title: String): String = {
    List(redirect, dependencyDotHeader(title)).mkString("\n\n")
  }

  def generate[A](buildDotSbt: String, filterRoot: Boolean = true)(f: (File, File) => A): A = {
    IO.withTemporaryDirectory { dir =>
      val project = dir / "project"
      IO.createDirectory(project)
      IO.write(project / "p.sbt", pluginSbtContents)
      IO.write(dir / "build.sbt", buildDotSbt)
      val args = new xsbt.boot.LauncherArguments("dependencyDot" :: Nil, false)
      println(xsbt.boot.Launch(dir, args))
      val dot = dir / "target" / "dependencies-compile.dot"
      if(filterRoot) {
        val dotLines = IO.readLines(dot)
        IO.write(dot, dotLines.filterNot(_.contains("default:sbt_")).mkString("\n"))
      }
      f(dir, dot)
    }
  }

  def generateFrom(title: String, additionalBuildSettings: String, graphType: GraphType, filterRoot: Boolean = true): graphType.A = {
    val buildDotSbt = defaultBuildDotSbt(title) + "\n\n" + additionalBuildSettings
    generate(buildDotSbt, filterRoot)(graphType.generate(_, _))
  }

  def withDependencies(dependencies: Seq[LibraryDependency], title: String, graphType: GraphType, filterRoot: Boolean = true): graphType.A =
    withTempDirAndDotFile(dependencies, title, filterRoot)(graphType.generate(_, _))

  private def linkText(link: LinkType): String = {
    link match {
      case LinkType.Redirect => redirect
      case LinkType.Embed => embed
      case LinkType.None => noLink
      case LinkType.Pom => pom
      case LinkType.Src => src
      case LinkType.Doc => doc
    }
  }

  def svgFromSettings(plainBuildSettings: String, title: String, link: LinkType, filterRoot: Boolean): String = {
    val buildDotSbt = List(linkText(link), dependencyDotHeader(title)).mkString("\n\n")
    generate(buildDotSbt + "\n\n" + plainBuildSettings, filterRoot)(GraphType.SVG.generate(_, _))
  }

  def svg(dependencies: Seq[LibraryDependency], title: String, link: LinkType, filterRoot: Boolean = true): String = {
    val buildDotSbt = List(linkText(link), dependencyDotHeader(title)).mkString("\n\n")
    generate(buildDotSbt + "\n\n" + dependencies.mkString("\n\n"), filterRoot)(GraphType.SVG.generate(_, _))
  }

  def withStdOut[A](action: => A): (Option[A], String) = {
    val encode = "UTF-8"
    val outStream = new ByteArrayOutputStream
    sbt.Using.bufferedOutputStream(outStream) { out =>
      val p = new PrintStream(out, true, encode)
      this.synchronized {
        val originalOut = System.out
        val originalErr = System.err
        try {
          System.setOut(p)
          System.setErr(p)
          try {
            val f = Future(action)(ExecutionContext.global)
            val r = Await.result(f, 29.seconds)
            p.flush()
            p.close()
            out.close()
            out.flush()
            Some(r) -> outStream.toString(encode)
          } catch {
            case NonFatal(e) =>
              e.printStackTrace()
              val oo = new ByteArrayOutputStream
              val pp = new PrintStream(oo, true, encode)
              e.printStackTrace(pp)
              None -> oo.toString(encode)
          }
        } finally {
          System.setOut(originalOut)
          System.setErr(originalErr)
          p.close()
        }
      }
    }
  }

  def metadataXmlFromCentral(groupId: String, artifactId: String): Option[Elem] =
    metadataXml("https://repo1.maven.org/maven2", groupId, artifactId)

  def metadataXml(baseUrl: String, groupId: String, artifactId: String): Option[Elem] =
    try {
      val url = s"$baseUrl/${groupId.replace('.', '/')}/$artifactId/maven-metadata.xml"
      Some(XML.load(url))
    } catch {
      case _: org.xml.sax.SAXParseException => // ignore
        None
      case NonFatal(e) =>
        e.printStackTrace()
        None
    }

  def findURL(pom: Elem): Option[String] = {
    List(
      pom \ "url",
      pom \ "scm" \ "url",
      pom \ "organization" \ "url"
    ).foldLeft(Option.empty[String]){
      case (None, elem) =>
        val url = elem.text
        if(url.startsWith("http")){
          Some(url)
        } else {
          None
        }
      case (a @ Some(_), _) =>
        a
    }
  }
}
