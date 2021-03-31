package EdmondKarps

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._


class EdmondKarpsTest{

  @Test
  def basicNetworkTest(): Unit = {

    for (i <- 1 until 5) {
      val n = 2
      val s = new GraphNode(n, 0)
      val t = new GraphNode(n, n-1)
      s.changeEdgeWeight(t, i)

      val ff = new EdmondKarps
      assert(ff(s, t, 2) == i, ff(s, t, 2))
    }

  }

  @Test
  def fourNodesTest():Unit = {
    val n = 4
    val s = new GraphNode(n, 0)
    val n1 = new GraphNode(n, 1)
    val n2 = new GraphNode(n, 2)
    val t = new GraphNode(n, n-1)
    s.changeEdgeWeight(n1, 151)
    s.changeEdgeWeight(n2, 150)
    n1.changeEdgeWeight(n2, 2)
    n1.changeEdgeWeight(t, 150)
    n2.changeEdgeWeight(t, 151)

    val ff = new EdmondKarps
    val result = ff(s, t, n)
    assert(result == 301, result)
  }

  @Test
  def fourNodes2Test():Unit = {
    val n = 4
    val s = new GraphNode(n, 0)
    val n1 = new GraphNode(n, 1)
    val n2 = new GraphNode(n, 2)
    val t = new GraphNode(n, n-1)
    s.changeEdgeWeight(n1, 151)
    s.changeEdgeWeight(n2, 200)
    n1.changeEdgeWeight(n2, 2)
    n1.changeEdgeWeight(t, 149)
    n2.changeEdgeWeight(t, 151)

    val ff = new EdmondKarps
    val result = ff(s, t, n)
    assert(result == 300, result)
  }

}
