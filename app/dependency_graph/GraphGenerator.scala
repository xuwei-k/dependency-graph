package dependency_graph

import org.joda.time.DateTime

abstract class GraphGenerator[A] {
  def get(dependencies: Seq[LibraryDependency], title: String, useCache: Boolean): Either[String, A]
}

object GraphGenerator{
  abstract class Cached[A] extends GraphGenerator[A] {
    def cacheMinute: Int
    def cacheSize: Int
    private[this] val cache = Cache.create[Set[LibraryDependency], A](cacheSize)

    def generate(dependencies: Seq[LibraryDependency], title: String): A

    override def get(dependencies: Seq[LibraryDependency], title: String, useCache: Boolean): Either[String, A] = {
      if(useCache) {
        DependencyGraph.withStdOut {
          generate(dependencies.toSeq, title)
        } match {
          case (Some(result), _) =>
            cache.put(dependencies.toSet, result, DateTime.now().plusMinutes(cacheMinute))
            println(cache)
            Right(result)
          case (None, stdout) =>
            Left(stdout)
        }
      } else {
        val result = generate(dependencies, title)
        cache.put(dependencies.toSet, result, DateTime.now().plusMinutes(cacheMinute))
        println(cache)
        Right(result)
      }
    }
  }

  class CachedSVG(val cacheMinute: Int, val cacheSize: Int) extends Cached[String]{
    override def generate(dependencies: Seq[LibraryDependency], title: String) =
      DependencyGraph.svg(dependencies, title)
  }

  class CachedPNG(val cacheMinute: Int, val cacheSize: Int) extends Cached[Array[Byte]]{
    override def generate(dependencies: Seq[LibraryDependency], title: String) =
      DependencyGraph.png(dependencies, title)
  }

  class CachedGIF(val cacheMinute: Int, val cacheSize: Int) extends Cached[Array[Byte]]{
    override def generate(dependencies: Seq[LibraryDependency], title: String) =
      DependencyGraph.gif(dependencies, title)
  }

  class CachedDOT(val cacheMinute: Int, val cacheSize: Int) extends Cached[String]{
    override def generate(dependencies: Seq[LibraryDependency], title: String) =
      DependencyGraph.dot(dependencies, title)
  }

  def gif(cacheMinute: Int, cacheSize: Int): GraphGenerator[Array[Byte]] =
    new CachedGIF(cacheMinute, cacheSize)

  def svg(cacheMinute: Int, cacheSize: Int): GraphGenerator[String] =
    new CachedSVG(cacheMinute, cacheSize)

  def png(cacheMinute: Int, cacheSize: Int): GraphGenerator[Array[Byte]] =
    new CachedPNG(cacheMinute, cacheSize)

  def dot(cacheMinute: Int, cacheSize: Int): GraphGenerator[String] =
    new CachedDOT(cacheMinute, cacheSize)
}
