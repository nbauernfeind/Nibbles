package com.nefariouszhen.ld23.entity.genome

import com.nefariouszhen.ld23.graphics.Screen
import com.nefariouszhen.ld23.entity.{Enemy, Player, Mob}

trait Shape {
  def xr: Int
  def yr: Int
  def render(screen: Screen, mob: Mob)
  def tick(mob: Mob)
}

class TwoStepShape(val xr: Int, val yr: Int, spriteOffset: Int, speedShift: Int) extends Shape {
  def render(screen: Screen, mob: Mob) {
    screen.render(mob.x - 4, mob.y - 4, ((mob.walkDist >> speedShift) & 1) + spriteOffset, 0, mob.getColor)
  }

  def tick(mob: Mob) {}
}

class JumpingShape(val xr: Int, val yr: Int, spriteOffset: Int, jumpInterval: Int, restInterval: Int) extends Shape {
  var (xa, ya) = (0, 0)
  var jumpTime = 0

  def enemyMoveLogic(mob: Mob) {
    if (mob.rand.nextInt(40) == 0 && jumpTime <= -restInterval) {
      xa = mob.rand.nextInt(3) - 1
      ya = mob.rand.nextInt(3) - 1

      val (dx, dy) = mob.distToPlayer
      if ((dx * dx + dy * dy) < mob.sightR2) {
        if (dx < 0) xa = -1
        if (dx > 0) xa = +1
        if (dy < 0) ya = -1
        if (dy > 0) ya = +1
      }
    }
  }

  def playerMoveLogic(p: Player) {
    if (p.input.up.down) ya = -1
    if (p.input.down.down) ya = 1
    if (p.input.left.down) xa = -1
    if (p.input.right.down) xa = 1
  }

  def tick(mob: Mob) {
    mob match {
      case e: Enemy => enemyMoveLogic(mob)
      case p: Player => playerMoveLogic(p)
    }

    if (jumpTime < 0 && (xa != 0 || ya != 0))
      jumpTime = jumpInterval

    mob.move(xa * mob.getSpeed, ya * mob.getSpeed)

    jumpTime -= mob.getSpeed
    if (jumpTime <= 0) {
      xa = 0
      ya = 0
    }
  }

  def render(screen: Screen, mob: Mob) {
    val t = if (jumpTime > 0) 1 else 0
    screen.render(mob.x - 4, mob.y - 4, t + spriteOffset, 0, mob.getColor)
  }
}

class Nibble extends TwoStepShape(3, 3, 0 + 3 * 20, 4)
class MiniBlob extends JumpingShape(3, 2, 2 + 3 * 20, 10, 10)
