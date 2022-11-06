package gdmitriev.testtask

import gdmitriev.testtask.graph.Digraph
import gdmitriev.testtask.graph.Path

class Questions(graph: Digraph[String]) {
  def task1 = abcDistance
  def task2 = adDistance
  def task3 = adcDistance
  def task4 = aebcdDistance
  def task5 = aedDistance
  def task6 = c2cLessOrEq3Stops
  def task7 = a2cEq4Stops
  def task8 = a2cShortest
  def task9 = b2bShortest
  def task10 = c2cDistanceLessThan30

  def abcDistance: Option[Int] = {
    for {
      ab <- graph.getEdge("A", "B")
      bc <- graph.getEdge("B", "C")
    } yield ab + bc
  }

  def adDistance: Option[Int] = {
    graph.getEdge("A", "D")
  }

  def adcDistance: Option[Int] = {
    for {
      ad <- graph.getEdge("A", "D")
      dc <- graph.getEdge("D", "C")
    } yield ad + dc
  }

  def aebcdDistance: Option[Int] = {
    for {
      ae <- graph.getEdge("A", "E")
      eb <- graph.getEdge("E", "B")
      bc <- graph.getEdge("B", "C")
      cd <- graph.getEdge("C", "D")
    } yield ae + eb + bc + cd
  }

  def aedDistance: Option[Int] = {
    for {
      ae <- graph.getEdge("A", "E")
      ed <- graph.getEdge("E", "D")
    } yield ae + ed
  }

  def c2cLessOrEq3Stops: Vector[Path[String]] = {
    graph.pathsWhile("C", "C")(_.vertices.length <= 4)
  }

  def a2cEq4Stops: Vector[Path[String]] = {
    graph.pathsWhile("A", "C")(_.vertices.length <= 5)
      .filter(_.vertices.length == 5)
  }

  def a2cShortest: Option[Path[String]] = {
    graph.shortestPath("A", "C")
  }

  def b2bShortest: Option[Path[String]] = {
    graph.shortestPath("B", "B")
  }

  def c2cDistanceLessThan30: Vector[Path[String]] = {
    graph.pathsWhile("C", "C")(_.weight < 30)
  }

}
