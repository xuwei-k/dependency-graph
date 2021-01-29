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
  licenses := Seq("MIT License" -> url("https://opensource.org/licenses/mit-license")),
  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
  shellPrompt := { state =>
    val branch = if(file(".git").exists){
      "git branch".lines_!.find{_.head == '*'}.map{_.drop(1)}.getOrElse("")
    }else ""
    Project.extract(state).currentRef.project + branch + " > "
  },
  fullResolvers ~= {_.filterNot(_.name == "jcenter")},
  resolvers += Resolver.url("typesafe", url("https://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns),
  resolvers += Opts.resolver.sonatypeReleases,
  libraryDependencies ++= (
    ("org.scala-sbt" % "sbt" % "0.13.12") :: // can't use 0.13.13
    ("com.github.xuwei-k" %% "play-json-extra" % "0.3.1") ::
    ("com.github.xuwei-k" %% "httpz-native" % "0.5.1") ::
    ("org.scalatest" %% "scalatest" % "3.0.3" % "test") ::
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
  ProjectRef(uri("git://github.com/sbt/launcher.git#v1.0.0"), "launcher-implementation")
)
