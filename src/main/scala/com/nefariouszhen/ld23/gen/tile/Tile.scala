package com.nefariouszhen.ld23.gen.tile

import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.gen.{Point, World}
import com.nefariouszhen.ld23.entity.Entity

sealed trait Tile {
  def render(screen: Screen, world: World, p: Point)
  def mayPass(world: World, p: Point, e: Entity): Boolean
}

object Tile {
  val EMPTY = new EmptyTile()
  val WALL = new WallTile()
  val FLOOR = new FloorTile()
  val UNKNOWN = EMPTY
}

private case class SubTile(dx: Int, dy: Int, tile: Int)

/* Each tile represents 4 sprite tiles.
  [ta tb]
  [tc td]
 */
abstract class SimpleTile(ta: Int, tb: Int, tc: Int, td: Int) extends Tile {
  private[this] val rs = List(
    SubTile(0, 0, ta),
    SubTile(8, 0, tb),
    SubTile(0, 8, tc),
    SubTile(8, 8, td)
  )

  def render(screen: Screen, world: World, p: Point) {
    rs.foreach(st => screen.render(p.x * 16 + st.dx, p.y * 16 + st.dy, st.tile, 0))
  }
}

class EmptyTile() extends SimpleTile(0,1,20,21) {
  def mayPass(world: World, p: Point, e: Entity) = false
}

//class WallTile() extends SimpleTile(2,3,22,23)
class WallTile() extends SimpleTile(0,1,20,21) {
  def mayPass(world: World, p: Point, e: Entity) = false
}

class FloorTile() extends SimpleTile(4,5,24,25) {
  def mayPass(world: World, p: Point, e: Entity) = true
}
