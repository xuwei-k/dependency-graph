package dependency_graph

import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.jsonext.CaseClassFormats

import scalaz.\/

final case class Gist(content: String) extends JsonToString[Gist]

object Gist{
  implicit val instance: Format[Gist] =
    CaseClassFormats.apply1(apply _, unapply _)("content")

  private[this] val fetchGistHeader: Map[String, String] =
    Env.GithubToken match {
      case Some(token) =>
        Map(("Authorization", "token " + token))
      case None =>
        Map.empty[String, String]
    }

  final class DecodeError(value: Seq[(JsPath, Seq[ValidationError])]) extends RuntimeException(value.toString)

  def fetch(gistId: String): Throwable \/ GistResponse = {
    val req = httpz.Request(
      url = Env.GithubApiBaseURL + "gists/" + gistId,
      headers = fetchGistHeader
    )

    val action = httpz.Core.string(req)
    println("start fetch gist " + gistId)
    for{
      str <- action.interpretBy(httpz.native.NativeInterpreter.sequential.empty.interpreter)
      json <- \/.fromTryCatchNonFatal(
        Json.parse(str)
      )
      r <- \/.fromEither(
        json.validate[GistResponse].asEither
      ).leftMap(new DecodeError(_))
    } yield r
  }
}
