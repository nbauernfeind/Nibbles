package com.nefariouszhen.ld23.entity

import genome._
import util.Random
import com.nefariouszhen.ld23.gen.{Direction, World}
import com.nefariouszhen.ld23.graphics.Screen

abstract class Mob(val world: World) extends Entity(world) {

  def bank: MemoryBank

  val defaultShape = new Nibble
  def getShape = bank.algos.collect({case ChangeShape(s) => s}).headOption.getOrElse(defaultShape)
  def getColor = bank.algos.collect({case ChangeColor(c) => c}).headOption.getOrElse(0)
  def getSpeed = 1 + bank.algos.collect({case SpeedBoost(s) => s}).sum
  def getSightR = bank.algos.collect({case SightBoost(s) => s}).sum
  def getMaxHealth = bank.algos.collect({case HealthBoost(h) => h}).sum + 1

  def xr = getShape.xr
  def yr = getShape.yr

  val rand = new Random()
  var walkDist = 0
  var dir: Direction = Direction.NORTH
  var maxHealth = 10
  var health = maxHealth
  var tickTime = 0

  def sightR2() = {
    val sr = getSightR
    ((4 * sr + 1) * (4 * sr + 1)).toInt
  }

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
