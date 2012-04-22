package com.nefariouszhen.ld23.entity

import util.Random
import com.nefariouszhen.ld23.gen.{Direction, World}

abstract class Mob(world: World) extends Entity(world) {
  xr = 4
  yr = 3

  val rand = new Random()
  var walkDist = 0
  var dir: Direction = Direction.NORTH
  var maxHealth = 10
  var health = maxHealth
  var tickTime = 0
  var sightR = 1

  def sightR2() = (16 * sightR + 1) * (16 * sightR + 1)

  override def tick() {
    tickTime += 1

    if (health <= 0) die()
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
}
