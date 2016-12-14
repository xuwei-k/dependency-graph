scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  "-Yno-adapted-args" ::
  Nil
)

fullResolvers ~= {_.filterNot(_.name == "jcenter")}

// can't use play 2.5 because does not support Scala 2.10
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.8")
