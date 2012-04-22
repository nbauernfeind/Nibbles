package com.nefariouszhen.ld23.entity

import genome.Shape
import util.Random
import com.nefariouszhen.ld23.gen.{Direction, World}
import com.nefariouszhen.ld23.graphics.Screen

abstract class Mob(val world: World) extends Entity(world) {
  def getShape: Shape
  def getColor: Int
  def getSpeed: Int

  def xr = getShape.xr
  def yr = getShape.yr

  val rand = new Random()
  var walkDist = 0
  var dir: Direction = Direction.NORTH
  var maxHealth = 10
  var health = maxHealth
  var tickTime = 0
  var sightR = 1

  def sightR2() = (16 * sightR + 1) * (16 * sightR + 1)

  def distToPlayer: Tuple2[Int, Int] = {
    (world.getPlayer.x - x, world.getPlayer.y - y)
  }

  override def tick() {
    tickTime += 1

    if (health <= 0) die()

    getShape.tick(this)
  }

  protected def die() {
    remove()
  }
  override def move(xa: Int, ya: Int): Boolean = {
    if (xa != 0 || ya != 0) {
      walkDist += 1
      dir = Direction.toDirection(xa, ya)
    }
    super.move(xa, ya)
  }

  def render(screen: Screen) {
    getShape.render(screen, this)
  }
}
