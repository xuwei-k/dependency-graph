import play.api.GlobalSettings
import play.api.mvc.{Results, RequestHeader}

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

}
