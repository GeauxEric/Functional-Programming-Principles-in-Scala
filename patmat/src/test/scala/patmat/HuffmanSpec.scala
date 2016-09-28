package patmat

import org.scalatest._
import Huffman._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HuffmanSpec extends FunSuite {
  trait TestTrees {
    val l1 = Leaf('a', 3)
    val l2 = Leaf('b', 8)
    val l3 = Leaf('c', 1)

    val f1 = makeCodeTree(l1, l2)
    val t = makeCodeTree(f1, l3)

    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    )

    val mychars = List('a', 'b', 'c', 'a', 'b')
  }

  test("weight: on a tree") {
    new TestTrees {
      assert(weight(t) === 12)
      assert(weight(sampleTree) === 4)
    }
  }

  test("chars: on a tree") {
    new TestTrees {
      assert(chars(t) contains('a'))
      assert(chars(t) contains('b'))
      assert(chars(t) contains('c'))
      assert(!(chars(t) contains('d')))
    }
  }

  test("times: on a list of chars") {
    new TestTrees {
      val myt = times(mychars)
      assert(myt contains(('a', 2)))
    }
  }

  test("combine: on a list of CodeTree") {
    new TestTrees {
      val c = combine(List(l1, l2, l3))
      assert(weight(c(1)) === 11)
    }
  }

  test("decode: one character") {
    new TestTrees {
      val tree = createCodeTree("aaaaaaaabbbcdefgh".toList)
      val a = decode(tree, List(0)).head
      assert(a == 'a')
    }
  }

}
