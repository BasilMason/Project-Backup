package automaton.automata

import automaton.garden.State

/**
  * Created by Basil on 09/08/2016.
  */

class CoordGrid(init: List[State], x: Int, y: Int, z: Int) extends Automata {

  type Cell = (State, Int)
  type Coordinate = (Int, Int, Int)

  require(x * y * z == init.length)

  private val radius = List(-1, 1)
  private val initWithIndex = init zipWithIndex
  private val coordinateTable: Map[Cell, Coordinate] = initWithIndex.map(in => (in, (if (x==0) 0 else in._2 % x, if (y==0) 0 else if (x==0) 0 else (in._2 / x), if (z==0) 0 else if (x==0 || y==0) 0 else (in._2 / (x * y)) % z))).toMap

  def clamp(i: Int): Int = if (i < 0) 0 else i

  // Von Neumann Neighbours
  def neighbours(in: List[Int]): Map[String, List[Int]] = {
    def h(acc: List[List[Int]], inp: List[Int], idx: Int): List[List[Int]] = idx match {
      case 3 => acc
      case n => h(acc ::: List(inp.updated(n, clamp(inp(n) - 1)), inp.updated(n, clamp(inp(n) + 1))), inp, n+1)
    }
    (List("LEFT", "RIGHT", "BACK", "FRONT", "BOTTOM", "TOP") zip h(List(), in, 0)).toMap
  }

  // get state and list of states
  // call rule
  // sequential and parallel


  override def next: List[State] = ???


  trait Site
  case object Top extends Site
  case object Bottom extends Site

}
