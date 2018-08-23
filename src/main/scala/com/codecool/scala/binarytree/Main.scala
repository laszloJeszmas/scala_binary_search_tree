package com.codecool.scala.binarytree

object Main {

  def main(args: Array[String]): Unit = {
    val binarySearchTree: BinarySearchTree = new BinarySearchTree
    var list: List[Int] = 5 :: 24 :: 1 :: 66 :: 446 :: 52 :: 9 :: 12 :: 35 :: 55 :: 53 :: 54 :: 72 :: 95 :: 30 :: 31 :: 75 :: 63 :: 49 :: Nil
    binarySearchTree.build(list)
    println(list.sorted)
    binarySearchTree.root
//    binarySearchTree.remove(66)
    binarySearchTree.remove(24)

    binarySearchTree.remove(1)
//    binarySearchTree.remove(49)

    println(list.sorted)

    println(list.sorted)

  }
}
