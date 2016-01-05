import play.api._
import play.api.http.DefaultHttpErrorHandler
import play.api.mvc.{RequestHeader, Results}

import scala.concurrent.Future

object Global extends GlobalSettings{

  override def onHandlerNotFound(request: RequestHeader) = {
    if (request.path.endsWith("/")) {
      val uri = request.path.take(request.path.length - 1) + {
        if (request.path == request.uri) ""
        else request.uri.substring(request.path.length)
      }
      Future.successful(Results.MovedPermanently(uri))
    } else {
      super.onHandlerNotFound(request)
    }
  }

  private[this] object ErrorHandler extends DefaultHttpErrorHandler(
    Environment.simple(mode = Mode.Dev),
    Configuration.empty,
    None,
    Some(router.Routes)
  )

  override def onError(request: RequestHeader, ex: Throwable) =
    ErrorHandler.onServerError(request, ex)
}
