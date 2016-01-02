package dependency_graph

import org.joda.time.DateTime

abstract class GraphGenerator[A, B] {
  def get(key: A, useCache: Boolean): Either[String, B]
}

object GraphGenerator{
  abstract class Cached[A, B](cacheMinute: Int, cacheSize: Int) extends GraphGenerator[A, B] {
    private[this] val cache = Cache.create[A, B](cacheSize)

    def generate(key: A): B

    override def get(key: A, useCache: Boolean): Either[String, B] = {
      if(useCache) {
        DependencyGraph.withStdOut {
          generate(key)
        } match {
          case (Some(result), _) =>
            cache.put(key, result, DateTime.now().plusMinutes(cacheMinute))
            println(cache)
            Right(result)
          case (None, stdout) =>
            Left(stdout)
        }
      } else {
        val result = generate(key)
        cache.put(key, result, DateTime.now().plusMinutes(cacheMinute))
        println(cache)
        Right(result)
      }
    }
  }

  def cached[A](cacheMinute: Int, cacheSize: Int, graphType: GraphType.Aux[A]): GraphGenerator[(Seq[LibraryDependency], String), A] =
    new Cached[(Seq[LibraryDependency], String), A](cacheMinute, cacheSize) {
      def generate(key: (Seq[LibraryDependency], String)): A =
        DependencyGraph.withDependencies(key._1, key._2, graphType)
    }

  def svg[A](cacheMinute: Int, cacheSize: Int): GraphGenerator[(Seq[LibraryDependency], String, LinkType), String] =
    new Cached[(Seq[LibraryDependency], String, LinkType), String](cacheMinute, cacheSize) {
      def generate(key: (Seq[LibraryDependency], String, LinkType)) =
        DependencyGraph.svg(key._1, key._2, key._3)
    }

}
