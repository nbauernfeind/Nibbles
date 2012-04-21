package com.nefariouszhen.ld23.gen.tile

import com.nefariouszhen.ld23.gen.World
import com.nefariouszhen.ld23.graphics.Screen

sealed trait Tile {
  def render(screen: Screen, world: World, x: Int, y: Int)
}

object Tile {
  val EMPTY = EmptyTile()
  val WALL = WallTile()
  val FLOOR = FloorTile()
  val UNKNOWN = EMPTY
}

private case class SubTile(dx: Int, dy: Int, tile: Int)

/* Each tile represents 4 sprite tiles.
  [ta tb]
  [tc td]
 */
class SimpleTile(ta: Int, tb: Int, tc: Int, td: Int) extends Tile {
  private[this] val rs = List(
    SubTile(0, 0, ta),
    SubTile(8, 0, tb),
    SubTile(0, 8, tc),
    SubTile(8, 8, td)
  )

  def render(screen: Screen, world: World, x: Int, y: Int) {
    rs.foreach(st => screen.render(x * 16 + st.dx, y * 16 + st.dy, st.tile, 0))
  }
}
case class EmptyTile() extends SimpleTile(0,1,20,21)
//case class WallTile() extends SimpleTile(2,3,22,23)
case class WallTile() extends SimpleTile(0,1,20,21)
case class FloorTile() extends SimpleTile(4,5,24,25)
