package UnionFind

class DisjointSetsArray(n:Int) extends DisjointSet{

  val representatives: Array[Int] = (0 until n).toArray

  def makeSet(x:Int):Unit = {
    representatives(x) = x
  }

  def findSet(x: Int): Int = representatives(x)

  def union(x: Int, y: Int): Unit = {
    if (findSet(x) != findSet(y)) {
      val x_rep = findSet(x)
      for (i <- 0 until n){
        if (findSet(i) == x_rep) representatives(i) = findSet(y)
      }
    }
  }
}

object DisjointSetsTest {
  def main(args:Array[String]) : Unit = {
    val sets = new DisjointSetsArray(11)
    sets.union(4,5)
    sets.union(8,9)
    println(sets.representatives.mkString(","))

    sets.union(4,8)

    println(sets.representatives.mkString(","))
  }
}

//case class LinkedList() {
//  var head:Node = null
//  var tail:Node = null
//}
//
//case class Node(val data:Int, var next:Node)
//
//class DisjointSetsLL(n:Int) extends DisjointSet {
//  var nodes: Array[Node] = for (i <- 0 until n) new Node(i, null)
//  def init() = {
//    var l:LinkedList = null
//    var node:Node = null
//    for (i <- 0 until n) {
//      l = new LinkedList()
//      node = new Node(i,null)
//      l.head = node; l.tail=node
//      representatives(i) = l
//    }
//  }
//
//  def findSet(x:Int):Int = {
//    representatives(x).head
//  }





//}
