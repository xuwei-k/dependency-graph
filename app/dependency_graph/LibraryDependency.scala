package dependency_graph

import play.api.libs.json.{Format, Json}

final case class LibraryDependency(
  groupId: String,
  artifactId: String,
  version: String
) {
  override def toString = s"""libraryDependencies += "$groupId" % "$artifactId" % "$version" """

  lazy val pomURL: String =
    s"https://repo1.maven.org/maven2/${groupId.replace('.', '/')}/${artifactId}/${version}/${artifactId}-${version}.pom"
}

object LibraryDependency {
  implicit val format: Format[LibraryDependency] = Json.format[LibraryDependency]
}
