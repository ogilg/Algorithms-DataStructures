package RedBlackTrees

import java.util.NoSuchElementException
import scala.annotation.tailrec


object RedBlackTree {
  type IsOrdered[A] = A => Ordered[A]

  class Tree[T : IsOrdered]
  case class Node[T : IsOrdered](colour : Colour, left : Tree[T], right : Tree[T], el:T) extends Tree[T]
  case class Empty[T : IsOrdered]() extends Tree[T]

  abstract class Colour
  case class Red() extends Colour
  case class Black() extends Colour

  // restructure the tree so that the invariants stay true
  def restructure[T : IsOrdered](colour:Colour, left:Tree[T], right:Tree[T], el:T): Tree[T] = {
    (colour, left, right, el) match {
        // case left red aligned
      case (Black(), Node(Red(), Node(Red(), a, b, x), c, y), d, z) => Node(Red(), Node(Black(), a, b, x), Node(Black(), c, d, z), y)
        // case left red not aligned
      case (Black(), Node(Red(), a, Node(Red(), b, c, x), y),d, z ) => Node(Red(), Node(Black(), a, b, x), Node(Black(), c, d, z), y)
        // case right red aligned
      case (Black(), a, Node(Red(), Node(Red(), b, c, x), d, y), z) => Node(Red(), Node(Black(), a, b, x), Node(Black(), c, d, z), y)
        // case right red not aligned
      case (Black(), a, Node(Red(), b, Node(Red(), c, d, x), y), z) => Node(Red(), Node(Black(), a, b, z), Node(Black(), c, d, x), y)
      case (c, l, r, el) => Node(c, l, r, el)
      case _ => throw new NoSuchElementException
    }
  }

  def insertRec[T : IsOrdered](a:T, t: Tree[T]): Tree[T] = {
    t match {
      case Node(c, l, r, el) =>
        if (el < a) {
          restructure(c, l, insertRec(a, r), el)
        }
        else {
          restructure(c, insertRec(a, l), r, el)
        }

      case Empty() => Node(Red(), Empty(), Empty(), a)
    }
  }

  def add[T : IsOrdered](a:T, t:Tree[T]): Tree[T] = {
    insertRec(a, t) match {
      case Node(_, l, r, el) => Node(Black(), l, r, el)
    }
  }

  @tailrec
  def isMember[T : IsOrdered](a:T, t:Tree[T]) : Boolean = {
    t match {
      case Node(_, l, r, el) =>
        if (el == a) true
        else if (el < a) isMember(a, r)
        else isMember(a, l)

      case Empty() => false
    }
  }

  def printTree[T](t:Tree[T]): Unit = {
    t match {
      case Node(c, l, r, a) =>
          print(c); println(a)
          print("Left: ")
          printTree(l)
          print("Right: ")
          printTree(r)
          println()
      case Empty() => println("Null")
    }
  }

  implicit class rbTree[T : IsOrdered](var tree: Tree[T]){
    def insert(v:T): Unit = tree = add(v, tree)
    def contains(v:T):Boolean = isMember(v, tree)
    def print(): Unit = printTree(tree)
  }

  // check that black depth is the same for all leaves
  def RBTreeTest(t:Tree[Int]): Unit = {
    println(RBTreeTestRec(t, 0))
    println("tree passed black depth test")
  }

  def RBTreeTestRec(t:Tree[Int], blackCount:Int): Int = {
    t match {
      case Node(c, l, r, _) =>
        var depthL = 0; var depthR = 0
        if (c == Black()) {
          depthL = RBTreeTestRec(l, blackCount + 1)
          depthR = RBTreeTestRec(r, blackCount + 1)
        } else {
          depthL = RBTreeTestRec(l, blackCount)
          depthR = RBTreeTestRec(r, blackCount)
        }
        assert(depthL == depthR)
        depthL
      case Empty() => blackCount
    }
  }


  def main(args:Array[String]) : Unit = {
    val t = new rbTree(new Node[Int](Black(), Empty(), Empty(), 3))
    t.insert(4)
    t.insert(10)
    t.insert(23)
    t.insert(7)
    t.insert(14)
    t.insert(16)
    t.insert(54)
    RBTreeTest(t.tree)
    t.insert(90)
    t.insert(19)
    t.insert(52)
    RBTreeTest(t.tree)
    t.insert(43)
    t.insert(75)
    t.insert(154)
    t.insert(26)
    t.insert(554)
    RBTreeTest(t.tree)
    t.print()
  }
}

