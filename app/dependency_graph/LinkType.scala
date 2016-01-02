package dependency_graph

sealed abstract class LinkType(val value: String) extends Product with Serializable

object LinkType {
  case object Redirect extends LinkType("redirect")
  case object Embed extends LinkType("embed")
  case object None extends LinkType("none")

  val all: Set[LinkType] = Set(Redirect, Embed, None)
  val map: Map[String, LinkType] = all.map(a => a.value -> a)(collection.breakOut)

  def unapply(name: String): Option[LinkType] = map.get(name)

  def from(value: Option[String]): Either[String, LinkType] = value match {
    case Some(LinkType(l)) =>
      Right(l)
    case scala.None =>
      Right(LinkType.Redirect)
    case Some(invalid) =>
      Left(invalid)
  }

}
