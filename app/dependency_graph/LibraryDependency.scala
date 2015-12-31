package dependency_graph

import play.api.libs.json.{Format, Json}

final case class LibraryDependency(
  groupId: String,
  artifactId: String,
  version: String
) {
  override def toString = s"""libraryDependencies += "$groupId" % "$artifactId" % "$version" """
}

object LibraryDependency {
  implicit val format: Format[LibraryDependency] = Json.format[LibraryDependency]
}
