package EdmondKarps

import EdmondKarps.GraphNode

import scala.collection.mutable
import util.control.Breaks._

/*
* Ford-Fulkerson Pseudocode*
* Set f(e) = 0 for all edges e in E
* Create residual graph, initialise as G
* While there is an augmenting path from s to t in residual graph
  * augment the flow along that path P
  * Update the residual graph
*/

class EdmondKarps {
  def apply(s:GraphNode, t: GraphNode, n:Int) : Int = {
    assert (n>=2, "need at least 2 nodes in graph")
    assert(s != null && t != null)
    var flow = 0

    while (true) {
      val q = new mutable.Queue[GraphNode]()
      q.enqueue(s)
      // prev map stores path so that we can backtrack when we find the target
      var prev = Map[GraphNode, GraphNode]()
      prev += (s -> null)

      while (q.nonEmpty && !prev.contains(t)) {
        val u = q.dequeue()
        for (v <- u.getSuccs) {
          if (!prev.contains(v)) {
            prev += (v -> u)
            q.enqueue(v)
          }
        }
      }

      // if no path was found, path does not exist
      if (!prev.contains(t)) {
        println("exiting and returning flow")
        return flow
      }
      println("path found")

      // find the weight of the bottleneck edge
      var bottleneck = Int.MaxValue
      var node = t
      while (!node.isSource) {
        val nextNode = node
        node = prev(node)
        bottleneck = List(bottleneck, node.getWeightTo(nextNode)).min
      }

      // update the residual graph
      node = t
      while (!node.isSource) {
        val nextNode = node
        node = prev(node)
        node.changeEdgeWeight(nextNode, -bottleneck)
      }

      flow += bottleneck

    }
    flow
  }
}

object EKTest {
  def main(args:Array[String]) : Unit = {
    val n = 2
    val s = new GraphNode(n, 0)
    val t = new GraphNode(n, n-1)

    s.changeEdgeWeight(t, 1)
    val ff = new EdmondKarps
    assert(ff(s, t, 2) == 1, ff(s,t,2))
  }
}

