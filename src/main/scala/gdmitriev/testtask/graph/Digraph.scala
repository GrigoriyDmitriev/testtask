package gdmitriev.testtask.graph

import scala.collection.mutable

trait Digraph[A] {
  def addVertex(a: A): Digraph[A]
  def addEdge(from: A, to: A, weight: Int): Digraph[A]
  def getEdge(from: A, to: A): Option[Int]

  def pathsWhile(from: A, to: A)(searchWhile: Path[A] => Boolean): Vector[Path[A]]
  def shortestPath(from: A, to: A): Option[Path[A]]
}

object Digraph {
  def empty[A]: Digraph[A] = impl.graph(Map.empty)

  private object impl {

    case class graph[A](paths: Map[A, Map[A, Int]]) extends Digraph[A] {
      override def addVertex(a: A): Digraph[A] = copy(paths + (a -> Map.empty))
      override def addEdge(from: A, to: A, weight: Int): Digraph[A] =
        copy(paths = paths.updatedWith(from) {
          case Some(value) => Some(value + (to -> weight))
          case None => Some(Map(to -> weight))
        })

      override def getEdge(from: A, to: A): Option[Int] = paths.get(from).flatMap(_.get(to))

      override def pathsWhile(from: A, to: A)(searchWhile: Path[A] => Boolean): Vector[Path[A]] = {
        import scala.collection.{mutable => mut}
        val pathsToGo = mut.Stack[Path[A]]()
        val result = Vector.newBuilder[Path[A]]
        def appendPaths(path: Path[A]): Unit = {
          paths(path.vertices.last).foreach { case (v, w) =>
            val p = Path(path.vertices.appended(v), path.weight + w)
            if (searchWhile(p)) {
              pathsToGo.push(p)
              if (v == to) {
                result.addOne(p)
              }
            }
          }
        }
        appendPaths(Path(Vector(from), 0))
        while (pathsToGo.nonEmpty) appendPaths(pathsToGo.pop)
        result.result()
      }

      override def shortestPath(from: A, to: A): Option[Path[A]] = {
        import scala.collection.{mutable => mut}
        val visited = mut.HashMap[A, Path[A]]()
        val visitQ = {
          implicit val pathOrd: Ordering[Path[A]] =
            Ordering[Int].reverse.on(_.weight)
          mut.PriorityQueue[Path[A]]()
        } // min q

        def visit(path: Path[A]): Unit = {
          paths(path.vertices.last).foreach { case (v, w) =>
            val newWeight = path.weight + w
            def proceedWithPath = {
              val p = Path(path.vertices.appended(v), newWeight)
              visited.addOne(v -> p)
              visitQ.addOne(p)
            }
            visited.get(v) match {
              case Some(value) =>
                if (value.weight > newWeight) proceedWithPath else ()
              case None => proceedWithPath
            }
          }
        }

        visit(Path(Vector(from), 0))
        // if (from == to) exclude it from visited
        // because we want to visit it again
        if (from == to) {
          visited.remove(from)
        }
        while (visitQ.nonEmpty) visit(visitQ.dequeue())

        visited.get(to)
      }
    }

  }
}
