package dependency_graph

sealed abstract class LinkType(val value: String) extends Product with Serializable

object LinkType {
  case object Redirect extends LinkType("redirect")
  case object Embed extends LinkType("embed")
  case object None extends LinkType("none")
  case object Pom extends LinkType("pom")
  case object Src extends LinkType("src")
  case object Doc extends LinkType("doc")

  val all: Set[LinkType] = Set(Redirect, Embed, None, Pom, Src, Doc)
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
