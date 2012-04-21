package com.nefariouszhen.ld23.gen

import tile.Tile
import util.Random
import collection.mutable.HashMap
import com.nefariouszhen.ld23.graphics.Screen

sealed trait Direction
object Direction {
  case object NORTH extends Direction
  case object SOUTH extends Direction
  case object EAST extends Direction
  case object WEST extends Direction

  val values = List(NORTH, SOUTH, EAST, WEST)
  def nextDir(r: Random): Direction = values(r.nextInt(values.size))
}

case class Point(x: Int, y: Int) {
  def move(d: Direction): Point = d match {
    case Direction.NORTH => Point(x, y + 1)
    case Direction.SOUTH => Point(x, y - 1)
    case Direction.EAST => Point(x - 1, y)
    case Direction.WEST => Point(x + 1, y)
  }

  def directionTo(p: Point): Direction = {
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

  def randPoint(rand: Random): Point = {
    Point(rand.nextInt(tx - bx - 1) + bx + 1, rand.nextInt(ty - by - 1) + by + 1)
  }
}

class World(val size: Int = 5) {
  private[this] val (minRS, maxRS) = (4, 8)

  val dimension = 1 << size
  private[this] val rand = new Random()
  private[this] val m = Array.ofDim[Tile](dimension, dimension)


  def getTile(x: Int, y: Int): Tile = if (!checkDimensions(Point(x, y))) Tile.UNKNOWN else Tile.FLOOR
  //  def getTile(x: Int, y: Int): Tile = if (!checkDimensions(Point(x, y))) Tile.UNKNOWN else m(x)(y)

  generate()

  def renderBackground(screen: Screen, xScroll: Int, yScroll: Int) {
    val (xo, yo) = (xScroll >> 4, yScroll >> 4)
    val (w, h) = ((screen.w + 15) >> 4, (screen.h + 15) >> 4)
    screen.offset = Point(xScroll, yScroll)
    for (y <- yo to h + yo; x <- xo to w + xo) {
      getTile(x, y).render(screen, this, x, y)
    }
  }

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

  private[this] def createRoom(c: Point, attempts: Int) {
    if (attempts <= 0) {
      return
    }

    // Generate room with borders on lines bx,by,tx,ty
    val (w, h) = (nextInt(minRS, maxRS), nextInt(minRS, maxRS))
    val (bx, by) = (c.x - w / 2, c.y - h / 2)
    val (tx, ty) = (bx + w, by + h)

    if (!checkDimensions(Point(bx, by)) || !checkDimensions(Point(tx, ty))) {
      return
    }

    for (i <- bx + 1 until tx; j <- by + 1 until ty) {
      m(i)(j) = Tile.FLOOR
    }

    for (j <- by to ty) {
      setToWallIfEmpty(Point(bx, j))
      setToWallIfEmpty(Point(tx, j))
    }

    for (i <- bx to tx) {
      setToWallIfEmpty(Point(i, by))
      setToWallIfEmpty(Point(i, ty))
    }

    var dirs = Direction.values
    var done = false

    while (!done) {
      var d = dirs(rand.nextInt(dirs.size))
      dirs = dirs.filter(_ == d)

      var p = d match {
        case Direction.NORTH => Point(nextInt(bx + 1, tx - 1), ty)
        case Direction.SOUTH => Point(nextInt(bx + 1, tx - 1), by)
        case Direction.EAST => Point(bx, nextInt(by + 1, ty - 1))
        case Direction.WEST => Point(tx, nextInt(by + 1, ty - 1))
      }

      val s = buildCorridor(p, d)
      createRoom(s, attempts - 1)

      done = rand.nextInt(100) > 10
    }
  }

  private[this] def buildCorridor(s: Point, d: Direction): Point = {
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

  private[this] def nextInt(min: Int, max: Int): Int = rand.nextInt(max - min) + min

  private[this] def createRoom(): Option[Room] = {
    // Generate room with borders on lines bx,by,tx,ty
    val (x, y) = (rand.nextInt(dimension), rand.nextInt(dimension))
    val (w, h) = (nextInt(minRS, maxRS), nextInt(minRS, maxRS))
    val (bx, by) = (x - w / 2, y - h / 2)
    val (tx, ty) = (bx + w, by + h)

    if (!checkDimensions(Point(bx, by)) || !checkDimensions(Point(tx, ty))) {
      return None
    }

    for (i <- bx + 1 until tx; j <- by + 1 until ty) {
      if (m(i)(j) == Tile.WALL) return None
    }

    for (i <- bx + 1 until tx; j <- by + 1 until ty) {
      m(i)(j) = Tile.FLOOR
    }

    for (j <- by to ty) {
      setToWallIfEmpty(Point(bx, j))
      setToWallIfEmpty(Point(tx, j))
    }

    for (i <- bx to tx) {
      setToWallIfEmpty(Point(i, by))
      setToWallIfEmpty(Point(i, ty))
    }

    Some(Room(bx, by, tx, ty))
  }

  private[this] def setToWallIfEmpty(p: Point) {
    if (checkDimensions(p) && m(p.x)(p.y) == Tile.EMPTY) {
      m(p.x)(p.y) = Tile.WALL
    }
  }

  private[this] def checkDimensions(p: Point): Boolean = (p.x >= 0 && p.x < dimension && p.y >= 0 && p.y < dimension)
}
