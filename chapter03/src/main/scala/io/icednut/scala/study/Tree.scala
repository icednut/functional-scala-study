package io.icednut.scala.study

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object TreeFarm {

  // 연습문제 3.25
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

  // 연습문제 3.26
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

  // 연습문제 3.27
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

  // 연습문제 3.28
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

  // 연습문제 3.29
  def fold[A, B](tree: Tree[A], initialValue: B)(f: (Tree[A], B) => B): B = {
    tree match {
      case Branch(l: Tree[A], r: Tree[A]) => {
        val result = f(tree, initialValue)
        val rightPath = fold(r, result)(f)

        val leftPath = fold(l, rightPath)(f)
        leftPath
      }
      case Leaf(v: A) => {
        f(Leaf(v), initialValue)
      }
    }
  }

  def sizeWithFold[A](tree: Tree[A], sizeResult: (Int, Int)) =
    fold(tree, (0: Int, 0: Int))((element, size) => element match {
      case Branch(_, _) => {
        val branchSize: Int = size._1 + 1
        val leafSize: Int = size._2
        (branchSize, leafSize)
      }
      case Leaf(_) => {
        val branchSize: Int = size._1
        val leafSize: Int = size._2 + 1
        (branchSize, leafSize)
      }
    })

  def maximumWithFold[A](tree: Tree[Int], maximumValue: Int) =
    fold(tree, 0)((element, maximumValue) => element match {
      case Branch(_, _) => maximumValue
      case Leaf(v) => v max maximumValue
    })

  def maxDepthWithFold[A](tree: Tree[A], depth: (Int, Int)) =
    fold(tree, (0: Int, 0: Int))((element, depth) => element match {
      case Branch(_, _) => {
        val newCurrentDepth = depth._1 + 1
        (newCurrentDepth, newCurrentDepth max depth._2)
      }
      case Leaf(_) => {
        val newCurrentDepth = depth._1 + 1
        (newCurrentDepth, newCurrentDepth max depth._2)
      }
    })
}
