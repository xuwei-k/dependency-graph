package dependency_graph

import org.joda.time.DateTime
import play.api.libs.json.{JsError, JsSuccess}
import play.api.mvc._

import scalaz.{\/, -\/, \/-}

object Application extends Controller {
  private[this] val cache = Cache.create[Set[LibraryDependency], String](100)
  private[this] val versionsCache = Cache.create[(String, String), List[String]](100)
  private[this] val artifactsCache = Cache.create[String, List[String]](1000)

  private[this] def toResult(either: Either[String, String]) =
    either match {
      case Right(svg) =>
        println("cache = " + cache)
        Ok(svg).as("image/svg+xml")
      case Left(stdout) =>
        InternalServerError(stdout)
    }

  val post = Action(parse.tolerantJson) { json =>
    json.body.validate[Set[LibraryDependency]] match {
      case JsSuccess(dependencies, _) =>
        toResult(run(dependencies))
      case e: JsError =>
        BadRequest(e.toString)
    }
  }

  def graph(g: String, a: String, v: String, useCache: Boolean) = Action {
    val set = Set(LibraryDependency(g, a, v))
    val result = if (useCache) {
      cache.get(set).map(Right(_)).getOrElse {
        run(set).right.map { svg =>
          cache.getOrElseUpdate(set, svg, DateTime.now().plusMinutes(30))
        }
      }
    } else {
      run(set)
    }
    toResult(result)
  }

  def versions(groupId: String, artifactId: String, useCache: Boolean) = Action {
    val list = versionsCache.getOrElseUpdate(
      (groupId, artifactId),
      DependencyGraph.versions(
        "http://repo1.maven.org/maven2",
        groupId,
        artifactId
      ),
      DateTime.now().plusMinutes(10)
    )

    val result = <html>
      <body>
        <ul>
          {list.map { v =>
          <li>
            <a href={s"http://dependency-graph.herokuapp.com/$groupId/$artifactId/$v"} target="_brank">
              {v}
            </a>
          </li>
        }}
        </ul>
      </body>
    </html>
    Ok(result).as(HTML)
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
            <li><a href={s"http://dependency-graph.herokuapp.com/$groupId/$a"} target="_blank">{a}</a></li>
          }
        }</ul>).as(HTML)
      case -\/(error) =>
        BadRequest(error.toString)
    }
  }

  def run(dependencies: Set[LibraryDependency]): Either[String, String] =
    DependencyGraph.withStdOut {
      DependencyGraph.generate(dependencies.toSeq)
    } match {
      case (Some(svg), _) =>
        cache.put(dependencies, svg, DateTime.now().plusMinutes(30))
        Right(svg)
      case (None, stdout) =>
        Left(stdout)
    }

}
