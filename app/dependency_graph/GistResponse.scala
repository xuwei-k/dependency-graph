package dependency_graph

import play.api.libs.json.Format
import play.jsonext.CaseClassFormats

final case class GistResponse(files: Map[String, Gist], description: String) extends JsonToString[GistResponse]

object GistResponse{
  implicit val instance: Format[GistResponse] =
    CaseClassFormats.apply2(apply _, unapply _)("files", "description")
}
