package UnionFind

trait DisjointSet{

  // Create new singleton {x}
  def makeSet(x:Int)

  //Return representative of set with x
  def findSet(x:Int) : Int

  //Merge set with x and set with y
  def union(x:Int, y: Int)
}
