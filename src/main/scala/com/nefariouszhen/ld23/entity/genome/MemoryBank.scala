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
  val sz = math.min(1, speed - 2)
}

case class SightBoost(sight: Int) extends StatAlgorithm {
  val sz = math.min(1, sight * sight / 2)
}

case class ArmorBoost(armor: Int) extends StatAlgorithm {
  val sz = math.min(1, armor / 2)
}

case class AttackBoost(attack: Int) extends StatAlgorithm {
  val sz = math.min(1, attack)
}

case class HealthBoost(health: Int) extends StatAlgorithm {
  val sz = health
}

sealed trait SpecialAlgorithm extends Algorithm {
  val sz = 4
}
case class DetectSight() extends SpecialAlgorithm
case class ElementalAttack() extends SpecialAlgorithm
case class RangeAttack() extends SpecialAlgorithm
case class PoisonAttack() extends SpecialAlgorithm
case class SplashAttack() extends SpecialAlgorithm
