package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val name = "independence"
    val t3 = createCodeTree(string2Chars(name))
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times check") {
    new TestTrees {
      assert(times(List('y', 'a', 'a', 's', 'h', 'u', 'u')) === List(('y', 1), ('a', 2), ('s', 1), ('h', 1), ('u', 2)))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("test function until") {
    val k = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))
    assert(until(singleton, combine)(k).toString === "{{{e(1)} ~ et(3) ~ {t(2)}} ~ etx(7) ~ {x(4)}}")
  }

  test("create code tree") {
    val name = "independence"
    assert(createCodeTree(string2Chars(name)).toString === "{{{{i(1)} ~ ip(2) ~ {p(1)}} ~ ipn(5) ~ {n(3)}} ~ ipncde(12) ~ {{{c(1)} ~ cd(3) ~ {d(2)}} ~ cde(7) ~ {e(4)}}}")

  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === "ab".toList)
      assert(decode(t3, List(0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1)) === "indepen".toList )
      assert(encode(t1)("ab".toList) === List(0, 1))
      assert(encode(t3)("indepen".toList) === List(0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1))
    }
  }

  test("quick encode") {
    new TestTrees {
      assert(encode(t3)("indepen".toList) === quickEncode(t3)("indepen".toList))
      assert(List(0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1) === quickEncode(t3)("indepen".toList))
      assert(encode(t1)("ab".toList) === quickEncode(t1)("ab".toList))
    }
  }

}
