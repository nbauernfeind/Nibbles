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

class World(val size: Int = 8) {
  private[this] val (minRS, maxRS) = (4, 6)
  private[this] val (minCL, maxCL) = (4, 8)

  val dimension = 1 << size
  private[this] val rand = new Random()
  private[this] val m = Array.ofDim[Tile](dimension, dimension)

  generate()

  def generate() {
    val center = Pos(dimension >> 1, dimension >> 1)

    for (i <- 0 until dimension; j <- 0 until dimension) {
      m(i)(j) = Tile.EMPTY
    }

    createRoom(center, 8)
  }

  def getTile(x: Int, y: Int): Tile = if (!checkDimensions(Pos(x, y))) Tile.UNKNOWN else m(x)(y)

  private[this] def nextInt(min: Int, max: Int): Int = rand.nextInt(max - min) + min

  private[this] def createRoom(c: Pos, attempts: Int) {
    if (attempts <= 0) {
      return
    }

    // Generate room with borders on lines bx,by,tx,ty
    val (w, h) = (nextInt(minRS, maxRS), nextInt(minRS, maxRS))
    val (bx, by) = (c.x - w / 2, c.y - h / 2)
    val (tx, ty) = (bx + w, by + h)

    if (!checkDimensions(Pos(bx, by)) || !checkDimensions(Pos(tx, ty))) {
      return
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

    var dirs = Direction.values
    var done = false

    while (!done) {
      var d = dirs(rand.nextInt(dirs.size))
      dirs = dirs.filter(_ == d)

      var p = d match {
        case Direction.NORTH => Pos(nextInt(bx + 1, tx - 1), ty)
        case Direction.SOUTH => Pos(nextInt(bx + 1, tx - 1), by)
        case Direction.EAST => Pos(bx, nextInt(by + 1, ty - 1))
        case Direction.WEST => Pos(tx, nextInt(by + 1, ty - 1))
      }

      val s = buildCorridor(p, d)
      createRoom(s, attempts - 1)

      done = rand.nextInt(100) > 10
    }
  }

  private[this] def buildCorridor(s: Pos, d: Direction): Pos = {
    val len = nextInt(minCL, maxCL)

    var p = s
    for (l <- 0 until len) {
      if (checkDimensions(p)) {
        m(p.x)(p.y) = Tile.FLOOR
        Direction.values.map(p.move _).filter(_ != p).foreach(setToWallIfEmpty _)
        p = p.move(d)
      }
    }

    rand.nextInt(100) match {
      case i: Int if i < 50 => p
      case _ => buildCorridor(p, Direction.values.filter(_ != d)(rand.nextInt(Direction.values.size - 1)))
    }
  }

  private[this] def setToWallIfEmpty(p: Pos) {
    if (checkDimensions(p) && m(p.x)(p.y) == Tile.EMPTY) {
      m(p.x)(p.y) = Tile.WALL
    }
  }

  private[this] def checkDimensions(p: Pos): Boolean = (p.x >= 0 && p.x < dimension && p.y >= 0 && p.y < dimension)
}
