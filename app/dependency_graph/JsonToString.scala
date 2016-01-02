package dependency_graph

import play.api.libs.json.{JsValue, Writes}

abstract class JsonToString[A <: JsonToString[A]](implicit A: Writes[A]) { self: A =>
  def toJson: JsValue = A.writes(this)
  override def toString = toJson.toString
}
