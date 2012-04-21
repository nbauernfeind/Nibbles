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
  val dimension = 1 << size
  private[this] val rand = new Random()
  private[this] val m = Array.ofDim[Tile](dimension, dimension)

  for (i <- 0 until dimension; j <- 0 until dimension) {
    m(i)(j) = Tile.EMPTY
  }
  createRoom(dimension >> 1, dimension >> 1, 4, 6, 5, 10, Direction.SOUTH, 20)

  def getTile(x: Int, y: Int): Tile = if (!checkDimensions(x, y)) Tile.UNKNOWN else m(x)(y)

  private[this] def nextInt(min: Int, max: Int): Int = rand.nextInt(max - min) + min

  private[this] def createRoom(x: Int, y: Int, minRS: Int, maxRS: Int, minCL: Int, maxCL: Int, dir: Direction, attempts: Int): Int = {
    if (attempts <= 0) return attempts
    var localAttempts = attempts

    // Generate room with borders on lines bx,by,tx,ty
    val (w, h) = (nextInt(minRS, maxRS), nextInt(minRS, maxRS))
    val (bx, by) = (x - w / 2, y - h / 2)
    val (tx, ty) = (bx + w, by + h)

    if (!checkDimensions(bx, by) || !checkDimensions(tx, ty)) {
      return localAttempts
    }

    for (i <- bx + 1 until tx; j <- by + 1 until ty) {
      m(i)(j) = Tile.FLOOR
    }

    for (j <- by to ty) {
      setToWallIfEmpty(bx, j)
      setToWallIfEmpty(tx, j)
    }

    for (i <- bx to tx) {
      setToWallIfEmpty(i, by)
      setToWallIfEmpty(i, ty)
    }

    var buildCorridor = true
    while (buildCorridor && localAttempts > 0) {
      val d = Direction.nextDir(rand)
      val len = nextInt(minCL, maxCL)

      var p = d match {
        case Direction.NORTH => Pos(nextInt(bx + 1, tx - 1), ty)
        case Direction.SOUTH => Pos(nextInt(bx + 1, tx - 1), by)
        case Direction.EAST => Pos(bx, nextInt(by + 1, ty - 1))
        case Direction.WEST => Pos(tx, nextInt(by + 1, ty - 1))
      }

      for (l <- 0 until len) {
        if (checkDimensions(p.x, p.y)) {
          m(p.x)(p.y) = Tile.UNKNOWN
          Direction.values.map(p.move _).filter(_ != p).foreach(setToWallIfEmpty _)
        }
        p = p.move(d)
      }

      buildCorridor = false
    }

    localAttempts
  }

  private[this] def setToWallIfEmpty(p: Pos): Unit = setToWallIfEmpty(p.x, p.y)

  private[this] def setToWallIfEmpty(x: Int, y: Int) {
    if (checkDimensions(x, y) && m(x)(y) == Tile.EMPTY) {
      m(x)(y) = Tile.WALL
    }
  }

  private[this] def checkDimensions(x: Int, y: Int): Boolean = (x >= 0 && x < dimension && y >= 0 && y < dimension)
}
