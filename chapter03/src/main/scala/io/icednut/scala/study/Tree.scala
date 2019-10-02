package io.icednut.scala.study

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object TreeFarm {

  def size[A](tree: Tree[A], sizeResult: (Int, Int)): (Int, Int) =
    tree match {
      case Branch(l, r) => {
        val leftSizeResult = size(l, (sizeResult._1, sizeResult._2))
        val rightSizeResult = size(r, (sizeResult._1, sizeResult._2))
        val branchSize = leftSizeResult._1 + rightSizeResult._1 + 1
        val leafSize = leftSizeResult._2 + rightSizeResult._2

        (branchSize, leafSize)
      }
      case Leaf(_) => {
        (sizeResult._1, sizeResult._2 + 1)
      }
    }

  def maximum(tree: Tree[Int], maximumValue: Int): Int = {
    tree match {
      case Branch(l, r) => {
        val leftMaximum = maximum(l, maximumValue)
        val rightMaximum = maximum(r, maximumValue)
        leftMaximum max rightMaximum
      }
      case Leaf(v) => {
        v max maximumValue
      }
    }
  }

  def maxDepth[A](tree: Tree[A], currentDepth: Int, maxDepthResult: Int): Int = {
    tree match {
      case Branch(l, r) => {
        val newCurerntDepth = currentDepth + 1
        val leftDepth = maxDepth(l, newCurerntDepth, maxDepthResult)
        val rightDepth = maxDepth(r, newCurerntDepth, maxDepthResult)

        leftDepth max rightDepth
      }
      case Leaf(_) => {
        val newCurerntDepth = currentDepth + 1

        newCurerntDepth max maxDepthResult
      }
    }
  }

  def map[A, B](tree: Tree[A], mapFunc: A => B): Tree[B] =
    tree match {
      case Branch(l, r) => {
        val leftBranch = map(l, mapFunc)
        val rightBranch = map(r, mapFunc)

        Branch(leftBranch, rightBranch)
      }
      case Leaf(v) => {
        Leaf(mapFunc(v))
      }
    }
}