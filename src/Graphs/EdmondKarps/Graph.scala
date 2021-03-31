package EdmondKarps

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Edge(toNode: GraphNode, weight: Int)

class GraphNode(size:Int, id:Int) {
  // outEdgeWeights stores the capacity of an edge from this node to all other nodes
  // capacity of 0 means no edge
  private val outEdgeWeights: mutable.Map[GraphNode, Int] = mutable.Map[GraphNode, Int]().withDefaultValue(0)

  // returns list of nodes reachable from this node
  def getSuccs: ListBuffer[GraphNode] = {
    val succs:ListBuffer[GraphNode] = ListBuffer()
    for (edge <- outEdgeWeights){
      if (edge._2 > 0) succs += edge._1
    }
    succs
  }

  def changeEdgeWeight(toNode: GraphNode, weight: Int): Unit = outEdgeWeights(toNode) += weight

  def getWeightTo(toNode:GraphNode) : Int = outEdgeWeights(toNode)

  def isSource:Boolean = id == 0

  def isTarget:Boolean = id == size-1
}
