package com.nefariouszhen.ld23.entity.genome

case class MemoryBank(sz: Int, algos: List[Algorithm]) {
  if (algos.map(_.sz).sum > sz) {
    println("Created memory bank with algos that don't fit...")
    println(this)
  }
}

sealed trait Algorithm {
  def sz: Int
}

sealed trait AestheticAlgorithm extends Algorithm {
  val sz = 1
}

case class ChangeShape(shape: Shape) extends AestheticAlgorithm
case class ChangeColor(col: Int) extends AestheticAlgorithm

sealed trait StatAlgorithm extends Algorithm

case class SpeedBoost(speed: Int) extends StatAlgorithm {
  val sz = math.max(1, speed - 2)
}

case class SightBoost(sight: Int) extends StatAlgorithm {
  val sz = math.max(1, sight - 1)
}

case class ArmorBoost(armor: Int) extends StatAlgorithm {
  val sz = math.max(1, armor / 2)
}

case class AttackBoost(attack: Int) extends StatAlgorithm {
  val sz = attack
}

case class HealthBoost(health: Int) extends StatAlgorithm {
  val sz = health
}

case class AttackRangeBoost(range: Int) extends StatAlgorithm {
  val sz = range / 2
}

sealed trait SpecialAlgorithm extends Algorithm {
  val sz = 4
}
case class DetectSight() extends SpecialAlgorithm
case class ElementalAttack() extends SpecialAlgorithm
case class RangeAttack() extends SpecialAlgorithm
case class PoisonAttack() extends SpecialAlgorithm
case class SplashAttack() extends SpecialAlgorithm
