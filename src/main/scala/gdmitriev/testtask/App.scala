package gdmitriev.testtask

import gdmitriev.testtask.graph.Digraph

import scala.io.StdIn.readLine

object App {
  import impl._
  // example input
  // AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7
  def main(args: Array[String]): Unit = {
    val input = readLine
    val edges = input.split(",")
      .map(_.trim)
      .map(parseEdge)
    val graph = edges.foldLeft(Digraph.empty[String]) { (graph, edge) =>
      graph.addEdge(edge.from, edge.to, edge.weight)
    }
    val q = new Questions(graph)
    println(q.task1.getOrElse("NO SUCH ROUTE"))
    println(q.task2.getOrElse("NO SUCH ROUTE"))
    println(q.task3.getOrElse("NO SUCH ROUTE"))
    println(q.task4.getOrElse("NO SUCH ROUTE"))
    println(q.task5.getOrElse("NO SUCH ROUTE"))
    println(q.task6.size)
    println(q.task7.size)
    println(q.task8.map(_.weight).getOrElse(-1))
    println(q.task9.map(_.weight).getOrElse(-1))
    println(q.task10.size)
  }

  private object impl {
    case class ParsedEdge(from: String, to: String, weight: Int)
    def parseEdge(s: String): ParsedEdge = {
      val f = s(0)
      val t = s(1)
      val w = s.substring(2).toInt
      ParsedEdge(f.toString, t.toString, w)
    }
  }
}
