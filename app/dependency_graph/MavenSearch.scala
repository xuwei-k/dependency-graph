package dependency_graph

import play.api.libs.json.{Format, Json}
import play.jsonext._

import scalaz.\/

final case class MavenSearch(response: Response) extends JsonToString[MavenSearch]

object MavenSearch{

  implicit val format: Format[MavenSearch] =
    CaseClassFormats(apply _, unapply _)(
      "response"
    )

  def searchByGroupId(groupId: String) = {
    import httpz._
    import httpz.native._

    val req = Request(
      url = "https://search.maven.org/solrsearch/select",
      params = Map(
        "q" -> s"g:$groupId",
        "rows" -> "256",
        "wt" -> "json"
      )
    )

    Core.bytes(req).interpret.flatMap(
      res => \/.fromEither(
        format.reads(Json.parse(res.value)).asEither
      ).map(_.response.docs.map(_.artifactId).sorted)
    )
  }

}


final case class Doc(
  artifactId: String,
  text: List[String]
) extends JsonToString[Doc]

object Doc {

  implicit val format: Format[Doc] =
    CaseClassFormats(apply _, unapply _)(
      "a",
      "text"
    )

}

final case class Response(
  docs: List[Doc]
) extends JsonToString[Response]

object Response {

  implicit val format: Format[Response] =
    CaseClassFormats(apply _, unapply _)(
      "docs"
    )

}
