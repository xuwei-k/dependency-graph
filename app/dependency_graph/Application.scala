package dependency_graph

import java.net.URL

import org.joda.time.DateTime
import play.api.http.Writeable
import play.api.libs.json.{JsError, JsSuccess}
import play.api.mvc._

import scalaz.{\/, -\/, \/-}
import scala.xml.Elem

object Application extends Controller {
  private object Generators {
    val png = GraphGenerator.png(60, 100)
    val svg = GraphGenerator.svg(60, 100)
    val gif = GraphGenerator.gif(60, 100)
    val dot = GraphGenerator.dot(60, 100)
  }
  private[this] val metadataCache = Cache.create[(String, String), Elem](100)
  private[this] val artifactsCache = Cache.create[String, List[String]](1000)
  private[this] val pomCache = Cache.create[LibraryDependency, Elem](1000)

  private[this] val IMAGE_SVG = "image/svg+xml"
  private[this] val IMAGE_PNG = "image/png"
  private[this] val IMAGE_GIF = "image/gif"

  private[this] def toResult[A: Writeable](either: Either[String, A], contentType: String) =
    either match {
      case Right(p) =>
        Ok(p).as(contentType)
      case Left(stdout) =>
        InternalServerError(stdout)
    }

  val post = Action(parse.tolerantJson) { json =>
    json.body.validate[Seq[LibraryDependency]] match {
      case JsSuccess(dependencies, _) =>
        toResult(Generators.svg.get(dependencies, "dependency graph", true), IMAGE_SVG)
      case e: JsError =>
        BadRequest(e.toString)
    }
  }

  def redirectProjectPage(g: String, a: String, v: String, useCache: Boolean) = Action {
    val l = LibraryDependency(g, a, v)
    val pom = if (useCache) {
      pomCache.getOrElseUpdate(
        l,
        scala.xml.XML.load(new URL(l.pomURL)),
        DateTime.now().plusMillis(120)
      )
    } else {
      scala.xml.XML.load(new URL(l.pomURL))
    }
    val url = DependencyGraph.findURL(pom).getOrElse(l.pomURL)
    Redirect(url)
  }

  def latest(groupId: String, artifactId: String, useCache: Boolean) = Action {
    val xml = if(useCache) {
      metadataWithCache(groupId, artifactId)
    } else {
      DependencyGraph.metadataXmlFromCentral(
        groupId,
        artifactId
      )
    }
    val latestVersion = (xml.toList \ "versioning" \ "latest").text
    if(latestVersion.nonEmpty) {
      Redirect(routes.Application.graph(groupId, artifactId, latestVersion, useCache))
    } else {
      NotFound(
        <p>{"not found latest version of "}<pre>{s""""$groupId" % "$artifactId""""}</pre></p>
      ).as(HTML)
    }
  }

  def graph(groupId: String, artifactId: String, version: String, useCache: Boolean) =
    svg(groupId, artifactId, version, useCache)

  def run[A: Writeable](l: LibraryDependency, useCache: Boolean, generator: GraphGenerator[A], contentType: String) = Action{
    val title = s"${l.groupId}/${l.artifactId}/${l.version} dependency graph"
    val result = generator.get(l :: Nil, title, useCache)
    toResult(result, contentType)
  }

  def gif(groupId: String, artifactId: String, version: String, useCache: Boolean) =
    run(LibraryDependency(groupId, artifactId, version), useCache, Generators.gif, IMAGE_GIF)

  def svg(groupId: String, artifactId: String, version: String, useCache: Boolean) =
    run(LibraryDependency(groupId, artifactId, version), useCache, Generators.svg, IMAGE_SVG)

  def png(groupId: String, artifactId: String, version: String, useCache: Boolean) =
    run(LibraryDependency(groupId, artifactId, version), useCache, Generators.png, IMAGE_PNG)

  def dot(groupId: String, artifactId: String, version: String, useCache: Boolean) =
    run(LibraryDependency(groupId, artifactId, version), useCache, Generators.dot, TEXT)

  private[this] def metadataWithCache(groupId: String, artifactId: String) = {
    val key = (groupId, artifactId)
    metadataCache.get(key).orElse{
      DependencyGraph.metadataXmlFromCentral(
        groupId,
        artifactId
      ).map{ x =>
        metadataCache.put(key, x, DateTime.now().plusMinutes(30))
        x
      }
    }
  }

  def versions(groupId: String, artifactId: String, useCache: Boolean) = Action {
    val xml = if(useCache){
      metadataWithCache(groupId, artifactId).toList
    }else{
      DependencyGraph.metadataXmlFromCentral(groupId, artifactId).toList
    }
    val list = (xml \\ "version").map(_.text).toList.sorted

    if(list.nonEmpty) {
      val urlList = {
        <li>
          <a href={routes.Application.latest(groupId, artifactId, useCache).url} target="_blank">latest</a>
        </li>
      } :: {
        list.map { v =>
          <li>
            <a href={routes.Application.graph(groupId, artifactId, v).url} target="_blank">
              {v}
            </a>
          </li>
        }
      }

      val result = <html>
        <body>
          <ul>
            {urlList}
          </ul>
        </body>
      </html>
      Ok(result).as(HTML)
    } else {
      val result = <p>{"Not found. See "}<a href={routes.Application.artifacts(groupId).url}>artifacts for "{groupId}"</a></p>
      NotFound(result).as(HTML)
    }
  }

  def artifacts(groupId: String, cache: Boolean) = Action{
    val result = if(cache) {
      artifactsCache.get(groupId).map(\/.right).getOrElse{
        MavenSearch.searchByGroupId(groupId).map{ a =>
          artifactsCache.put(groupId, a, DateTime.now().plusMinutes(60))
          a
        }
      }
    } else {
      MavenSearch.searchByGroupId(groupId)
    }

    result match {
      case \/-(res) =>
        Ok(<ul>{
          res.map{ a =>
            <li><a href={routes.Application.versions(groupId, a, cache).url} target="_blank">{a}</a></li>
          }
        }</ul>).as(HTML)
      case -\/(error) =>
        BadRequest(error.toString)
    }
  }

  val favicon = Action{
    Ok("").as(BINARY)
  }
}
