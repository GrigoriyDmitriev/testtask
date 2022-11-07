package gdmitriev.testtask

import gdmitriev.testtask.graph.{Digraph, Path}
import utest._

object ShortestPathSpec extends TestSuite {
  val tests = Tests {
    test("connected graph") {
      val graph = Digraph.empty[Int]
        .addEdge(1, 2, 7).addEdge(2, 1, 7)
        .addEdge(1, 3, 9).addEdge(3, 1, 9)
        .addEdge(2, 3, 10).addEdge(3, 2, 10)
        .addEdge(2, 4, 15).addEdge(4, 2, 15)
        .addEdge(3, 4, 11).addEdge(4, 3, 11)
        .addEdge(4, 5, 6).addEdge(5, 4, 6)
        .addEdge(5, 6, 9).addEdge(6, 5, 9)
        .addEdge(3, 6, 2).addEdge(6, 3, 2)
        .addEdge(1, 6, 14).addEdge(6, 1, 14)
      assert(graph.shortestPath(1, 5) == Some(
        Path(Vector(1, 3, 6, 5), 20)
      ))
    }

    test("disconnected graph") {
      val graph = Digraph.empty[Int]
        .addEdge(1, 2, 5).addEdge(2, 1, 5)
        .addEdge(3, 4, 6).addEdge(4, 3, 6)
      assume(graph.shortestPath(1, 3).isEmpty)
      assume(graph.shortestPath(4, 2).isEmpty)
    }
  }
}
