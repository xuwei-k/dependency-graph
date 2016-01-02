package dependency_graph

import java.io.File

import play.api.mvc.{Results, Result}

import scala.sys.process.Process
import sbt.Path._

sealed abstract class GraphType(val id: String) extends Product with Serializable {
  val idOpt: Option[String] = Some(id)
  type A
  def generate(tempDir: File, dot: File): A
  def asPlayResult(a: A): Result
}

object GraphType {
  type Aux[X] = GraphType{ type A = X }

  private def binary(tempDir: File, dot: File, format: String) = {
    val outputFile = tempDir / "graph"
    Process(Seq("dot", "-o" + outputFile.getAbsolutePath, "-T" + format, dot.getAbsolutePath), tempDir).!(DependencyGraph.logger)
    sbt.IO.readBytes(outputFile)
  }

  case object SVG extends GraphType("svg") {
    type A = String
    override def generate(tempDir: File, dot: File) = {
      val svg = tempDir / "graph.svg"
      Process(Seq("dot", "-o" + svg.getAbsolutePath, "-Tsvg", dot.getAbsolutePath), tempDir).!(DependencyGraph.logger)
      sbt.IO.read(svg)
    }

    override def asPlayResult(a: String) =
      Results.Ok(a).as("image/svg+xml")
  }

  case object DOT extends GraphType("dot") {
    type A = String
    override def generate(tempDir: File, dot: File) = {
      sbt.IO.read(dot)
    }

    override def asPlayResult(a: String) =
      Results.Ok(a)
  }

  case object GIF extends GraphType("gif") {
    type A = Array[Byte]
    override def generate(tempDir: File, dot: File) =
      binary(tempDir, dot, "gif")

    override def asPlayResult(a: Array[Byte]) =
      Results.Ok(a).as("image/gif")
  }

  case object PNG extends GraphType("png") {
    type A = Array[Byte]
    override def generate(tempDir: File, dot: File) =
      binary(tempDir, dot, "png")

    override def asPlayResult(a: Array[Byte]) =
      Results.Ok(a).as("image/png")
  }

  val all: Set[GraphType] = Set(SVG, GIF, PNG, DOT)
  val map: Map[String, GraphType] = all.map(a => a.id -> a)(collection.breakOut)

  def unapply(name: String): Option[GraphType] =
    map.get(name)

  def parseWithDefault(name: Option[String]): Either[String, GraphType] =
    name match {
      case Some(GraphType(x)) =>
        Right(x)
      case None =>
        Right(GraphType.SVG)
      case Some(invalid) =>
        Left(invalid)
    }
}
