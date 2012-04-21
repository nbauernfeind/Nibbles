package com.nefariouszhen.ld23.gen

import util.Random

trait Direction
object Direction {
  case object NORTH extends Direction
  case object SOUTH extends Direction
  case object EAST extends Direction
  case object WEST extends Direction

  val values = List(NORTH, SOUTH, EAST, WEST)
  def nextDir(r: Random): Direction = values(r.nextInt(values.size))
}

trait Tile
object Tile {
  case object EMPTY extends Tile
  case object WALL extends Tile
  case object FLOOR extends Tile
}

class World(val size: Int = 10) {
  val dimension = 1 << size
  val center = dimension >> 1
  val rand = new Random()
  val m = Array.ofDim[Tile](dimension, dimension)

  for (i <- 0 until dimension; j <- 0 until dimension) {
    m(i)(j) = Tile.EMPTY
  }
  createRoom(center, center, 4, 6, 20, 22, Direction.SOUTH, 20)

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
      //      val d = Direction.nextDir(rand)
      buildCorridor = false
    }

    localAttempts
  }

  private[this] def setToWallIfEmpty(x: Int, y: Int) {
    if (checkDimensions(x, y) && m(x)(y) == Tile.EMPTY) {
      m(x)(y) = Tile.WALL
    }
  }

  private[this] def checkDimensions(x: Int, y: Int): Boolean = (x >= 0 && x < dimension && y >= 0 && y < dimension)
}
