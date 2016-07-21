package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val emptyHeap = const(empty)
  val nonEmptyHeap = for {
    a <- arbitrary[Int]
    g <- genHeap
  } yield insert(a, g)

  lazy val genHeap: Gen[H] = Gen.oneOf(emptyHeap, nonEmptyHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll {
      (a: Int) =>
          val h = insert(a, empty)
          findMin(h) == a
  }

  property("findMin1") = forAll(Gen.choose(0, 1000)) {
    (a: Int) =>
          val h = insert(a+1, insert(a+2, insert(a, empty)))
          findMin(h) == a
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    (h1 != empty && h2 != empty) ==> {
    val h = meld(h1, h2)  
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    if(m1 < m2) findMin(h) == m1
    else findMin(h) == m2
    }
  }

  property("deleteMin1") = forAll { (a: Int) =>
    val h = insert(a+4, insert(a+5, insert(a+3, empty)))
    val hmin = findMin(h)
    findMin(deleteMin(h)) != hmin
  }
}
