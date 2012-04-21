package com.nefariouszhen.ld23.gen

import util.Random
import collection.mutable.HashMap

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

  def directionTo(p: Pos): Direction = {
    if (p.x > x) {
      Direction.WEST
    } else if (p.x < x) {
      Direction.EAST
    } else if (p.y > y) {
      Direction.NORTH
    } else {
      Direction.SOUTH
    }
  }
}

case class Room(bx: Int, by: Int, tx: Int, ty: Int) {
  def dist(r: Room): Int = {
    val dx = math.max(0, math.min(tx, r.tx) - math.max(bx, r.bx))
    val dy = math.max(0, math.min(ty, r.ty) - math.max(by, r.by))
    dx + dy
  }

  def randPoint(rand: Random): Pos = {
    Pos(rand.nextInt(tx - bx - 1) + bx + 1, rand.nextInt(ty - by - 1) + by + 1)
  }
}

class World(val size: Int = 5) {
  private[this] val (minRS, maxRS) = (4, 8)

  val dimension = 1 << size
  private[this] val rand = new Random()
  private[this] val m = Array.ofDim[Tile](dimension, dimension)

  def getTile(x: Int, y: Int): Tile = if (!checkDimensions(Pos(x, y))) Tile.UNKNOWN else Tile.FLOOR
//  def getTile(x: Int, y: Int): Tile = if (!checkDimensions(Pos(x, y))) Tile.UNKNOWN else m(x)(y)

  generate()

  def generate() {
    for (i <- 0 until dimension; j <- 0 until dimension) {
      m(i)(j) = Tile.EMPTY
    }

    val rooms = (0 until 120).flatMap(_ => createRoom())
    val start = rooms.head
    val dist = HashMap[Room, Int]()

    rooms.foreach(r => dist.put(r, start.dist(r)))

    var nr = rooms.tail

//    while (nr.length > 0) {
//      val nextRoom = nr.sortBy(dist(_)).head
//      nr = nr.filter(_ != nextRoom)
//      val d = dist.remove(nextRoom).get
//      nr.foreach(r => dist.put(r, math.min(dist(r), nextRoom.dist(r))))
//
//      val valid = rooms.filter(r => !nr.contains(r) && r.dist(nextRoom) <= d).head
//      val (a, b) = (valid.randPoint(rand), nextRoom.randPoint(rand))
//      createCorridor(a, b.copy(y = a.y))
//      createCorridor(a.copy(x = b.x), b)
//    }
  }

  private[this] def createCorridor(a: Pos, b: Pos) {
    val d = a.directionTo(b)
    var p = a
    while (p != b) {
      checkDimensions(p)
      m(p.x)(p.y) = Tile.FLOOR
      Direction.values.map(p.move _).foreach(setToWallIfEmpty _)
      p = p.move(d)
    }
  }

  private[this] def nextInt(min: Int, max: Int): Int = rand.nextInt(max - min) + min

  private[this] def createRoom(): Option[Room] = {
    // Generate room with borders on lines bx,by,tx,ty
    val (x, y) = (rand.nextInt(dimension), rand.nextInt(dimension))
    val (w, h) = (nextInt(minRS, maxRS), nextInt(minRS, maxRS))
    val (bx, by) = (x - w / 2, y - h / 2)
    val (tx, ty) = (bx + w, by + h)

    if (!checkDimensions(Pos(bx, by)) || !checkDimensions(Pos(tx, ty))) {
      return None
    }

    for (i <- bx + 1 until tx; j <- by + 1 until ty) {
      if (m(i)(j) == Tile.WALL) return None
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

    Some(Room(bx, by, tx, ty))
  }

  private[this] def setToWallIfEmpty(p: Pos) {
    if (checkDimensions(p) && m(p.x)(p.y) == Tile.EMPTY) {
      m(p.x)(p.y) = Tile.WALL
    }
  }

  private[this] def checkDimensions(p: Pos): Boolean = (p.x >= 0 && p.x < dimension && p.y >= 0 && p.y < dimension)
}
