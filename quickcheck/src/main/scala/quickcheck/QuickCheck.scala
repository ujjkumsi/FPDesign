package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min
import Math.max

import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty),
    for {
      i <- arbitrary[Int]
      m <- frequency((1, Gen.const(empty)), (9, genHeap))
    } yield insert(i,m)
  )


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: A =>
    findMin(insert(a, empty)) == a
  }

  property("min2") = forAll { (a: A, b:A) =>
    findMin(insert(b, insert(a, empty))) == min(a,b)
  }

  property("deleteMin") = forAll { a: A =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("deleteToGetSortedList") = forAll { h: H =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    }
    isSorted(h)
  }

  property("meldHeap") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if(isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h1)
    else if(isEmpty(h1)) findMin(meld(h1, h2)) == findMin(h2)
    else findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("meld with minimal check") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val m1 = findMin(h1)
      val m = min(m1, findMin(h2))
      findMin(meld(deleteMin(h1), insert(m1, h2))) == m
    }
  }

  property("equal heap if all min are same") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) || isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    isEmpty(h1) || isEmpty(h2) || heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)



}
