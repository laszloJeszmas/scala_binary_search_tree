package com.codecool.scala.binarytree

import java.util.NoSuchElementException

import scala.annotation.tailrec

class BinarySearchTree {

  var root: Option[Node] = None: Option[Node]

  def build(listToSort: List[Int]): Unit = {
    val list = listToSort.sorted
    val size = list.size
    lazy val middleIndex = findMiddleIndex()

    def findMiddleIndex(): Option[Int] = {
      if (size == 0) None
      else if (size == 1) Some(0)
      else Some(Math.floor(size/2).asInstanceOf[Int])
    }

    val leftSide = list.slice(0, size / 2)
    val rightSide = list.slice(size / 2 + 1, size)

    if (middleIndex.isDefined) {
      add(new Node(list(middleIndex.get)))
      build(leftSide)
      build(rightSide)
    }
  }

  def add(node: Node): Unit ={
    if (root.isEmpty) {
      this.root = Some(node)

    } else {
      var parent: Option[Node] = this.root
      addNode()

      @tailrec
      def addNode(): Unit ={
        if (parent.get.data == node.data) throw new IllegalArgumentException

        else if (parent.get.data > node.data) {
          parent.get.leftChild match {
            case Some(n) => parent = parent.get.leftChild
              addNode()
            case None => parent.get.leftChild = Some(node)
          }
        }

        else {
          parent.get.rightChild match {
            case Some(n) => parent = parent.get.rightChild
              addNode()
            case None => parent.get.rightChild = Some(node)
          }
        }
      }
    }
  }

  def find(element: Int): Boolean ={
    var parent: Option[Node] = this.root

    @tailrec
    def findElement(): Boolean ={
      parent match {
        case None => false
        case Some(n) =>
          if (n.data == element) {
            return true
          } else if (n.data > element) {
            parent = n.leftChild
          } else if (n.data < element) {
            parent = n.rightChild
          }
            findElement()
      }
    }
    findElement()
  }

  def remove(element: Int): Unit = {
    var parent: Option[Node] = this.root
    if (parent.isEmpty) throw new NoSuchElementException()
    if (parent.get.data == element) {
      val nodeToRemove: Node = parent.get
    } else {
      lazy val nodeToRemove: Node = findElement()


      @tailrec
      def findElement(): Node = {
        val node: Node = parent.get
        if (node.data < element) {
          if (node.rightChild.isDefined && node.rightChild.get.data == element) node.rightChild.get
          else if (node.rightChild.isDefined) {
            parent = node.rightChild
            findElement()
          }
          else throw new NoSuchElementException

        } else {
          if (node.leftChild.isDefined && node.leftChild.get.data == element) node.leftChild.get
          else if (node.leftChild.isDefined) {
            parent = node.leftChild
            findElement()
          }
          else throw new NoSuchElementException
        }
      }

      if (parent.get.rightChild.get == nodeToRemove) {
        val replaceNode: Option[Node] = findLastRightChild(nodeToRemove)
        if (replaceNode.isDefined) {
          parent.get.rightChild = replaceNode
          replaceNode.get.rightChild = nodeToRemove.rightChild
        } else {
          parent.get.rightChild = nodeToRemove.rightChild
        }
      } else {
        val replaceNode: Option[Node] = findLastLeftChild(nodeToRemove)
        if (replaceNode.isDefined) {
          parent.get.leftChild = replaceNode
          replaceNode.get.leftChild = nodeToRemove.leftChild
        } else {
          parent.get.leftChild = nodeToRemove.leftChild
        }
      }
    }
        def findLastLeftChild(node: Node): Option[Node] = {
          if (node.rightChild.isEmpty) return None
          var child: Option[Node] = node.rightChild


          def findChild(): Option[Node] = {
            while (child.get.leftChild.isDefined) {
              child = child.get.leftChild
            }
            if (child.get.rightChild.isEmpty) child
            else {
              child = child.get.rightChild
              findChild()
            }
          }
          findChild()
        }

        def findLastRightChild(node: Node): Option[Node] = {
          if (node.leftChild.isEmpty) return None
          var child: Option[Node] = node.leftChild


          def findChild(): Option[Node] = {
            while (child.get.rightChild.isDefined) {
              child = child.get.rightChild
            }
            if (child.get.leftChild.isEmpty) child
            else {
              child = child.get.leftChild
              findChild()
            }
          }
          findChild()
        }
      }
}
