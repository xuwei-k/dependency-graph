package dependency_graph

import org.joda.time.DateTime

abstract class GraphGenerator[A] {
  def get(dependencies: Seq[LibraryDependency], title: String, useCache: Boolean): Either[String, A]
}

object GraphGenerator{
  abstract class Cached[A](cacheMinute: Int, cacheSize: Int) extends GraphGenerator[A] {
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

  def cached[A](cacheMinute: Int, cacheSize: Int, graphType: GraphType.Aux[A]): GraphGenerator[A] =
    new Cached[A](cacheMinute, cacheSize) {
      def generate(dependencies: Seq[LibraryDependency], title: String): A =
        DependencyGraph.withDependencies(dependencies, title, graphType)
    }

}
