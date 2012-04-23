package com.nefariouszhen.ld23.entity

import genome._
import com.nefariouszhen.ld23.gen.{Direction, World}
import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.gen.Direction._
import particle.{AttackParticle, TextParticle}

abstract class Mob(val world: World) extends Entity(world) {

  def bank: MemoryBank

  val defaultShape = new Nibble
  def getShape = bank.algos.collect({case ChangeShape(s) => s}).headOption.getOrElse(defaultShape)
  def getColor = bank.algos.collect({case ChangeColor(c) => c}).headOption.getOrElse(0)
  def getSpeed = 1 + bank.algos.collect({case SpeedBoost(s) => s}).sum
  def getSightR = bank.algos.collect({case SightBoost(s) => s}).sum
  def getMaxHealth = 1 + bank.algos.collect({case HealthBoost(h) => h}).sum
  def getAttackDamage = 1 + bank.algos.collect({case AttackBoost(a) => a}).sum
  def getAttackRange = 2*(2 + bank.algos.collect({case AttackRangeBoost(a) => a}).sum)

  override def xr = getShape.xr
  override def yr = getShape.yr

  var walkDist = 0
  var dir: Direction = Direction.NORTH
  var health = 10000
  var tickTime = 0
  var hurtTime = 0
  var xKnockback = 0
  var yKnockback = 0

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
    if (health > getMaxHealth)
      health = getMaxHealth

    if (hurtTime > 0)
      hurtTime -= 1

    getShape.tick(this)
  }

  protected def die() {
    remove()
  }

  protected def attack() {
    val yo = -2
    val range = getAttackRange

    world.add(new AttackParticle(this, world, range, dir))

    dir match {
      case NORTH => hurtEntities(x - 8, y - range + yo, x + 8, y - 4 + yo)
      case SOUTH => hurtEntities(x - 8, y + 4 + yo, x + 8, y + range + yo)
      case EAST => hurtEntities(x - range, y - 8 + yo, x - 4, y + 8 + yo)
      case WEST => hurtEntities(x + 4, y - 8 + yo, x + range, y + 8 + yo)
    }
  }

  private[this] def hurtEntities(x0: Int, y0: Int, x1: Int, y1: Int) {
    for (e <- world.getEntities(x0, y0, x1, y1)) {
      if (e != this) {
        e.hurt(this, getAttackDamage, dir)
      }
    }
  }

  override def hurt(mob: Mob, damage: Int, attackDir: Direction) {
    doHurt(damage, attackDir)
  }

  private[this] def doHurt(damage: Int, attackDir: Direction) {
    if (hurtTime > 0) return

    //    val (dx,dy) = distToPlayer
    //    if (dx*dx + dy*dy < 80*80) {
    //      sound hurt
    //    }

    world.add(new TextParticle(world, "" + damage, x, y, 0xff0000))

    health -= damage
    attackDir match {
      case NORTH => yKnockback = +6
      case SOUTH => yKnockback = -6
      case EAST => xKnockback = -6
      case WEST => xKnockback = +6
    }
  }

  override def move(xa: Int, ya: Int): Boolean = {
    if (xKnockback < 0) {
      move2(-1, 0)
      xKnockback += 1
    }
    if (xKnockback > 0) {
      move2(1, 0)
      xKnockback -= 1
    }
    if (yKnockback < 0) {
      move2(0, -1)
      yKnockback += 1
    }
    if (yKnockback > 0) {
      move2(0, 1)
      yKnockback -= 1
    }
    if (hurtTime > 0) return true

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
