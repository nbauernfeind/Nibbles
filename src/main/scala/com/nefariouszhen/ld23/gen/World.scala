package com.nefariouszhen.ld23.gen

import util.Random

sealed trait Direction
object Direction {
  case object NORTH extends Direction
  case object SOUTH extends Direction
  case object EAST extends Direction
  case object WEST extends Direction

  val values = List(NORTH, SOUTH, EAST, WEST)
  def nextDir(r: Random): Direction = values(r.nextInt(values.size))
}

sealed trait Tile
object Tile {
  case object EMPTY extends Tile
  case object WALL extends Tile
  case object FLOOR extends Tile
  case object UNKNOWN extends Tile
}

case class Pos(x: Int, y: Int) {
  def move(d: Direction): Pos = d match {
    case Direction.NORTH => Pos(x, y + 1)
    case Direction.SOUTH => Pos(x, y - 1)
    case Direction.EAST => Pos(x - 1, y)
    case Direction.WEST => Pos(x + 1, y)
  }
}

class World(val size: Int = 6) {
  private[this] val (minRS, maxRS) = (4, 6)
  private[this] val (minCL, maxCL) = (5, 10)

  val dimension = 1 << size
  private[this] val rand = new Random()
  private[this] val m = Array.ofDim[Tile](dimension, dimension)

  for (i <- 0 until dimension; j <- 0 until dimension) {
    m(i)(j) = Tile.EMPTY
  }

  val center = Pos(dimension >> 1, dimension >> 1)
  createRoom(center, 20)

  def getTile(x: Int, y: Int): Tile = if (!checkDimensions(Pos(x, y))) Tile.UNKNOWN else m(x)(y)

  private[this] def nextInt(min: Int, max: Int): Int = rand.nextInt(max - min) + min

  private[this] def createRoom(p: Pos, attempts: Int): Int = {
    if (attempts <= 0) return attempts
    var localAttempts = attempts

    // Generate room with borders on lines bx,by,tx,ty
    val (w, h) = (nextInt(minRS, maxRS), nextInt(minRS, maxRS))
    val (bx, by) = (p.x - w / 2, p.y - h / 2)
    val (tx, ty) = (bx + w, by + h)

    if (!checkDimensions(Pos(bx, by)) || !checkDimensions(Pos(tx, ty))) {
      return localAttempts
    }

    for (i <- bx + 1 until tx; j <- by + 1 until ty) {
      m(i)(j) = Tile.FLOOR
    }

    for (j <- by to ty) {
      setToWallIfEmpty(Pos(bx, j))
      setToWallIfEmpty(Pos(tx, j))
    }

    for (i <- bx to tx) {
      setToWallIfEmpty(Pos(i, by))
      setToWallIfEmpty(Pos(i, ty))
    }

    var buildCorridor = true
    while (buildCorridor && localAttempts > 0) {
      localAttempts -= 1

      val d = Direction.nextDir(rand)
      val len = nextInt(minCL, maxCL)

      var p = d match {
        case Direction.NORTH => Pos(nextInt(bx + 1, tx - 1), ty)
        case Direction.SOUTH => Pos(nextInt(bx + 1, tx - 1), by)
        case Direction.EAST => Pos(bx, nextInt(by + 1, ty - 1))
        case Direction.WEST => Pos(tx, nextInt(by + 1, ty - 1))
      }

      for (l <- 0 until len) {
        if (checkDimensions(p)) {
          m(p.x)(p.y) = Tile.FLOOR
          Direction.values.map(p.move _).filter(_ != p).foreach(setToWallIfEmpty _)
          buildCorridor = false
        } else {
          buildCorridor = true
        }
        p = p.move(d)
      }

      if (!buildCorridor) {
        val attemptsLeft = createRoom(p, localAttempts)
        if (attemptsLeft == localAttempts) buildCorridor = true
        localAttempts = attemptsLeft
      }
    }

    localAttempts
  }

  private[this] def setToWallIfEmpty(p: Pos) {
    if (checkDimensions(p) && m(p.x)(p.y) == Tile.EMPTY) {
      m(p.x)(p.y) = Tile.WALL
    }
  }

  private[this] def checkDimensions(p: Pos): Boolean = (p.x >= 0 && p.x < dimension && p.y >= 0 && p.y < dimension)
}
