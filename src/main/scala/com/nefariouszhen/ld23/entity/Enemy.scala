package com.nefariouszhen.ld23.entity

import com.nefariouszhen.ld23.gen.World
import genome._

class Enemy(world: World, lvl: Int) extends Mob(world) {
  val bankSz = 3 + lvl
  val bank = new MemoryBank(bankSz, generateAlgos(bankSz))

  def generateAlgos(_sz: Int): List[Algorithm] = {
    var sz = _sz
    val shape = ChangeShape (rand.nextInt(2) match {
      case 0 => new MiniBlob
      case 1 => new TallBlob
    })
    sz -= shape.sz

    val color = ChangeColor(rand.nextInt())
    sz -= color.sz

    var ups = List[Algorithm]()
    for (i <- 0 until 100) {
      val powerUp = if (rand.nextInt(100) < 95) {
        // Stat Boost
        rand.nextInt(5) match {
          case 0 => SpeedBoost(rand.nextInt(4)+1)
          case 1 => SightBoost(rand.nextInt(2)+1)
          case 2 => ArmorBoost(rand.nextInt(10)+1)
          case 3 => AttackBoost(rand.nextInt(10)+1)
          case 4 => HealthBoost(rand.nextInt(5)+1)
        }
      } else {
        rand.nextInt(5) match {
          case 0 => DetectSight()
          case 1 => ElementalAttack()
          case 2 => RangeAttack()
          case 3 => PoisonAttack()
          case 4 => SplashAttack()
        }
      }

      if (sz >= powerUp.sz) {
        ups = powerUp :: ups
        sz -= powerUp.sz
      }
    }

    List(shape, color) ++ ups
  }
}
