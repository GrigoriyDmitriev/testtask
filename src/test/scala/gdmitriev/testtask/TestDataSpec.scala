package gdmitriev.testtask

import gdmitriev.testtask.graph.{Digraph, Path}
import utest._

object TestDataSpec extends TestSuite {
  val tests = Tests {
    val graph = Digraph.empty[String]
      .addEdge("A", "B", 5)
      .addEdge("B", "C", 4)
      .addEdge("C", "D", 8)
      .addEdge("D", "C", 8)
      .addEdge("D", "E", 6)
      .addEdge("A", "D", 5)
      .addEdge("C", "E", 2)
      .addEdge("E", "B", 3)
      .addEdge("A", "E", 7)

    val q = new Questions(graph)

    test("Task 1: A-B-C distance") {
      assert(q.task1 == Some(5 + 4))
    }
    test("Task 2: A-D distance") {
      assert(q.task2 == Some(5))
    }
    test("Task 3: A-D-C distance") {
      assert(q.task3 == Some(5 + 8))
    }
    test("Task 4: A-E-B-C-D distance") {
      assert(q.task4 == Some(7 + 3 + 4 + 8))
    }
    test("Task 5: A-E-D distance") {
      assert(q.task5 == None) // no path E-D
    }
    test("Task 6: number of trips from C to C with max 3 stops") {
      val trips = q.task6.toSet
      assert(trips == Set(
        Path(Vector("C", "E", "B", "C"), 9),
        Path(Vector("C", "D", "C"), 16)
      ))
    }
    test("Task 7: number of trips from A to C with 4 stops") {
      val trips = q.task7.toSet
      assert(trips == Set(
        Path(Vector("A", "D", "E", "B", "C"), 18),
        Path(Vector("A", "D", "C", "D", "C"), 29),
        Path(Vector("A", "B", "C", "D", "C"), 25)
      ))
    }
    test("Task 8: length of shortest route from A to C") {
      assert(q.task8 == Some(Path(Vector("A", "B", "C"), 9)))
    }
    test("Task 9: length of shortest route from B to B") {
      assert(q.task9 == Some(Path(Vector("B", "C", "E", "B"), 9)))
    }
    test("Task 10: number of routes from C to C with distance <30") {
      val trips = q.task10.toSet
      assert(trips == Set(
        Path(Vector("C", "D", "C"), 16),
        Path(Vector("C", "E", "B", "C"), 9),
        Path(Vector("C", "D", "E", "B", "C"), 21),
        Path(Vector("C", "E", "B", "C", "D", "C"), 25),
        Path(Vector("C", "D", "C", "E", "B", "C"), 25),
        Path(Vector("C", "E", "B", "C", "E", "B", "C"), 18),
        Path(Vector("C", "E", "B", "C", "E", "B", "C", "E", "B", "C"), 27)
      ))
    }
  }
}
