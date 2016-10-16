package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(x, heap)

  lazy val positiveInt = arbitrary[Int] suchThat(_ > 0)

  lazy val genPositiveHeap: Gen[H] = for {
    positive_val <- positiveInt
    heap <- oneOf(const(empty), genPositiveHeap)
  } yield insert(positive_val, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin of one node") = forAll { (_: H) =>
    val oneNode = insert(0, empty)
    findMin(oneNode) == 0
  }

  property("findMin of two values") = forAll { (m: Int, n: Int, h: H) =>
    val min_val = if (isEmpty(h)) m min n else (m min n) min (findMin(h))
    min_val == findMin(insert(m, insert(n, h)))
  }

  property("delete one after insterting one") = forAll { (m: Int, h: H) =>
    if (isEmpty(h)) {
      isEmpty(deleteMin(insert(m, h)))
    }
    else {
      val min_val = m min (findMin(h))
      min_val == findMin(insert(m, h))
    }
  }

  property("findMin of heap of all positive values inserted with one negative value") =
    forAll(genPositiveHeap) { (heap: H) =>
      val inserted = insert(-1, heap)
      findMin(inserted) == -1
    }

  property("min of the merged") = forAll { (m: H, n: H) =>
    if (isEmpty(m)) findMin(meld(m, n)) == findMin(n)
    else if (isEmpty(n)) findMin(meld(m, n)) == findMin(m)
    else {
      val min_m = findMin(m)
      val min_n = findMin(n)
      val min_val = min_n min min_m
      findMin(meld(m, n)) == min_val
    }
  }

  property("heap sort") = forAll { (l: List[Int]) =>
    if (l.isEmpty) true
    else {
      def List2Heap(l: List[Int], h: H): H = {
        if (l.isEmpty) h
        else List2Heap(l.tail, insert(l.head, h))
      }

      def Heap2List(h: H): List[Int] = {
        if (isEmpty(h)) Nil
        else {
          val min_val = findMin(h)
          val rest_h = deleteMin(h)
          min_val :: Heap2List(rest_h)
        }
      }

      val heap = List2Heap(l, empty)
      val sorted = Heap2List(heap)
      l.sortWith((n1, n2) => n1 < n2) == sorted
    }
  }
}
