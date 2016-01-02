package dependency_graph

import java.io.File

import play.api.mvc.{Results, Result}

import scala.sys.process.Process
import sbt.Path._

sealed abstract class GraphType extends Product with Serializable {
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

  case object SVG extends GraphType {
    type A = String
    override def generate(tempDir: File, dot: File) = {
      val svg = tempDir / "graph.svg"
      Process(Seq("dot", "-o" + svg.getAbsolutePath, "-Tsvg", dot.getAbsolutePath), tempDir).!(DependencyGraph.logger)
      sbt.IO.read(svg)
    }

    override def asPlayResult(a: String) =
      Results.Ok(a).as("image/svg+xml")
  }

  case object DOT extends GraphType {
    type A = String
    override def generate(tempDir: File, dot: File) = {
      sbt.IO.read(dot)
    }

    override def asPlayResult(a: String) =
      Results.Ok(a)
  }

  case object GIF extends GraphType {
    type A = Array[Byte]
    override def generate(tempDir: File, dot: File) =
      binary(tempDir, dot, "gif")

    override def asPlayResult(a: Array[Byte]) =
      Results.Ok(a).as("image/gif")
  }

  case object PNG extends GraphType {
    type A = Array[Byte]
    override def generate(tempDir: File, dot: File) =
      binary(tempDir, dot, "png")

    override def asPlayResult(a: Array[Byte]) =
      Results.Ok(a).as("image/png")
  }

  def unapply(name: String): Option[GraphType] =
    PartialFunction.condOpt(name) {
      case "svg" => SVG
      case "gif" => GIF
      case "png" => PNG
      case "dot" => DOT
    }
}
