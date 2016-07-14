package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val lv = Vector(Vector('-', '-', 'o', 'o'), Vector('o', 'S', 'T', '-'), Vector('o', 'o', 'o', 'o'))

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("check terrainFunction") {
    new Level1 {
      val fun = terrainFunction(lv)
      assert(fun(Pos(1, 2)) === true)
      assert(fun(Pos(0, 0)) === false)
    }
  }

  test("check findChar") {
    new Level1 {
      assert(findChar('S', lv) === Pos(1, 1))
      assert(findChar('T', lv) === Pos(1, 2))
    }
  }

  test("== on Pos") {
    new Level1 {
    val p = Pos(3, 4)
    assert(Pos(1, 2) == Pos(1, 2))
    assert(Pos(2, 3) != Pos(4, 7))
    assert(Pos(3, 4) == p)
    }
  }




	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
