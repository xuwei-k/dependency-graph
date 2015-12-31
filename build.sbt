lazy val root = Project(
  "dependency-graph", file(".")
).enablePlugins(PlayScala).settings(
  // SHOULD NOT specify scalaVersion due to sbtDependency
  scalacOptions ++= (
    "-deprecation" ::
    "-unchecked" ::
    "-Xlint" ::
    "-language:existentials" ::
    "-language:higherKinds" ::
    "-language:implicitConversions" ::
    Nil
  ),
  javaOptions ++= sys.process.javaVmArguments.filter(
    a => Seq("-Xmx", "-Xms", "-XX").exists(a.startsWith)
  ),
  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
  shellPrompt := { state =>
    val branch = if(file(".git").exists){
      "git branch".lines_!.find{_.head == '*'}.map{_.drop(1)}.getOrElse("")
    }else ""
    Project.extract(state).currentRef.project + branch + " > "
  },
  fullResolvers ~= {_.filterNot(_.name == "jcenter")},
  resolvers += Resolver.url("typesafe", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns),
  resolvers += Opts.resolver.sonatypeReleases,
  libraryDependencies ++= (
    ("org.scala-sbt" % "sbt" % sbtVersion.value) ::
    ("org.scalatest" %% "scalatest" % "2.2.5" % "test") ::
    Nil
  ),
  mappings in Universal ~= { m =>
    // workaround for "Duplicate mappings" error
    type T = List[(File, String)]
    @annotation.tailrec
    def distinct(src: T, set: Set[String], acc: T): T = src match {
      case h :: t =>
        if(set(h._2)){
          distinct(t, set, acc)
        }else{
          distinct(t, set + h._2, h :: acc)
        }
      case Nil =>
        acc.reverse
    }
    distinct(m.toList, Set.empty, Nil)
  }
).dependsOn(
  ProjectRef(uri("git://github.com/sbt/launcher.git#v1.0.0-M1"), "launcher-implementation")
)
