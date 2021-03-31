import scala.reflect.ClassTag

class DynamicTable[T](startSize:Int)(implicit m: ClassTag[T]){
  // Invariant:
  // 1/4 < loadFactor < 1
  // loadFactor = numEntries/tableSize

  var tableSize: Int = startSize
  var table = new Array[T](startSize)
  private var loadFactor = 0
  var numEntries = 0

  def insert(x:T):Unit = {
    if (numEntries == tableSize){
      resizeTable(tableSize*2)
    }
    table(numEntries) = x
    numEntries += 1
    updateLoadFactor()
  }

  def deleteEntry(): Unit = {
    if(numEntries > 0) numEntries -= 1
    if (loadFactor <= 1/4) resizeTable(tableSize/2)
    updateLoadFactor()
  }

  private def resizeTable(newSize:Int): Unit = {
    var newTable = new Array[T](newSize)
    var i = 0
    while (i < newSize && i < numEntries){
      newTable(i) = table(i)
      i+=1
    }
    table = newTable
    tableSize = newSize
  }

  private def updateLoadFactor():Unit = {
    loadFactor = numEntries/tableSize
  }
}


object DynamicTableTest {
  def main(args:Array[String]):Unit = {
    val dtable = new DynamicTable[Int](2)

    dtable.insert(2)
    dtable.insert(2)
    println(dtable.tableSize)
    dtable.insert(2)
    dtable.insert(2)
    println(dtable.tableSize)
    dtable.insert(2)
    println(dtable.tableSize)

    dtable.deleteEntry()
    dtable.deleteEntry()
    println(dtable.tableSize)




  }
}
