package io.icednut.scala.study

import org.scalatest.FunSuite

/**
 * @author will.109
 * @date 03/10/2019
 **/
class TreeFarmTest extends FunSuite {

  test("[연습문제 3.25] 트리의 잎과 가지의 개수를 셀 수 있어야 한다.") {
    val tree = Branch(
      Branch(
        Leaf("a"),
        Leaf("b")
      ),
      Branch(
        Leaf("c"),
        Leaf("d")
      )
    )
    val sizeResult = TreeFarm.size(tree, (0, 0))
    assert(sizeResult == (3, 4))
  }

  test("[연습문제 3.25] 트리의 잎과 가지의 개수를 셀 수 있어야 한다. 2") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf("a"),
          Leaf("aa")
        ),
        Leaf("b")
      ),
      Branch(
        Leaf("c"),
        Leaf("d")
      )
    )
    val sizeResult = TreeFarm.size(tree, (0, 0))
    assert(sizeResult == (4, 5))
  }

  test("[연습문제 3.26] Int 트리에서 가장 큰 요소를 구할 수 있어야 한다.") {
    val givenMaxValue = 212
    val tree: Tree[Int] = Branch(
      Branch(
        Branch(
          Leaf(1),
          Leaf(givenMaxValue)
        ),
        Leaf(11)
      ),
      Branch(
        Leaf(5),
        Leaf(87)
      )
    )
    val result = TreeFarm.maximum(tree, 0)
    assert(result == givenMaxValue)
  }

  test("[연습문제 3.27] 트리의 루트에서 가장 깊은 리프까지의 경로의 길이를 구할 수 있어야 한다.") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf("a"),
          Leaf("aa")
        ),
        Leaf("b")
      ),
      Branch(
        Leaf("c"),
        Leaf("d")
      )
    )
    val maxDepth = TreeFarm.maxDepth(tree, 0, 0)
    assert(maxDepth == 4)
  }

  test("[연습문제 3.27] 트리의 루트에서 가장 깊은 리프까지의 경로의 길이를 구할 수 있어야 한다. 2") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf("a"),
          Leaf("aa")
        ),
        Leaf("b")
      ),
      Branch(
        Leaf("c"),
        Branch(
          Branch(
            Leaf("d"),
            Leaf("e")
          ),
          Branch(
            Branch(
              Branch(
                Leaf("i"),
                Branch(
                  Leaf("j"),
                  Branch(
                    Leaf("k"),
                    Branch(
                      Leaf("l"),
                      Leaf("m")
                    )
                  )
                )
              ),
              Leaf("h")
            ),
            Branch(
              Leaf("f"),
              Leaf("g")
            )
          )
        )
      )
    )
    val maxDepth = TreeFarm.maxDepth(tree, 0, 0)
    assert(maxDepth == 10)
  }

  test("[연습문제 3.28] map 함수를 통해 트리의 value를 변경할 수 있어야 한다.") {
    val tree = Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Leaf("a"),
        Leaf("b")
      )
    )
    val mappedTree = TreeFarm.map(tree, v => "hi, " + v)

    mappedTree match {
      case Branch(l, r) => {
        l match {
          case Branch(ll, lr) => {
            ll match {
              case Leaf(v) => assert(v == "hi, 1")
            }
            lr match {
              case Leaf(v) => assert(v == "hi, 2")
            }
          }
        }
        r match {
          case Branch(rl, rr) => {
            rl match {
              case Leaf(v) => assert(v == "hi, a")
            }
            rr match {
              case Leaf(v) => assert(v == "hi, b")
            }
          }
        }
      }
    }
  }

  test("[연습문제 3.29] fold 함수로 size를 구현해보자.") {
    val tree: Tree[String] = Branch(
      Branch(
        Leaf("a"),
        Leaf("b")
      ),
      Branch(
        Leaf("c"),
        Leaf("d")
      )
    )

    val result = TreeFarm.sizeWithFold(tree, (0: Int, 0: Int))
    assert(result == (3, 4))
  }

  test("[연습문제 3.29] fold 함수로 maximum을 구현해보자.") {
    val givenMaxValue = 212
    val tree: Tree[Int] = Branch(
      Branch(
        Branch(
          Leaf(1),
          Leaf(givenMaxValue)
        ),
        Leaf(11)
      ),
      Branch(
        Leaf(5),
        Leaf(87)
      )
    )

    val result = TreeFarm.maximumWithFold(tree, 0)
    assert(result == givenMaxValue)
  }

  test("[연습문제 3.29] fold 함수로 maxDepth를 구현해보자.") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf("a"),
          Leaf("aa")
        ),
        Leaf("b")
      ),
      Branch(
        Leaf("c"),
        Leaf("d")
      )
    )

    val maxDepth = TreeFarm.maxDepthWithFold(tree, (0, 0))
    assert(maxDepth._2 == 4)
  }
}
