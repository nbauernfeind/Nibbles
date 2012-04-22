package com.nefariouszhen.ld23.gen

import tile.Tile
import util.Random
import com.nefariouszhen.ld23.graphics.Screen
import java.util.ArrayList
import scala.collection.JavaConversions._
import com.nefariouszhen.ld23.entity.{Player, Entity}
import collection.IterableProxy

sealed trait Direction
object Direction {
  case object NORTH extends Direction
  case object SOUTH extends Direction
  case object EAST extends Direction
  case object WEST extends Direction

  val values = List(NORTH, SOUTH, EAST, WEST)
  def nextDir(r: Random): Direction = values(r.nextInt(values.size))

  def toDirection(dx: Int, dy: Int): Direction = {
    if (dx > 0) WEST
    else if (dx < 0) EAST
    else if (dy > 0) SOUTH
    else NORTH
  }
}

object Point {
  def toPoint(xScroll: Int, yScroll: Int) = Point(xScroll >> 4, yScroll >> 4)
  def nextPoint(r: Random, w: Int, h: Int) = Point(r.nextInt(w), r.nextInt(h))
}

case class Point(x: Int, y: Int) {
  def move(d: Direction): Point = d match {
    case Direction.NORTH => Point(x, y - 1)
    case Direction.SOUTH => Point(x, y + 1)
    case Direction.EAST => Point(x - 1, y)
    case Direction.WEST => Point(x + 1, y)
  }

  def directionTo(p: Point): Direction = {
    if (p.x > x) {
      Direction.WEST
    } else if (p.x < x) {
      Direction.EAST
    } else if (p.y < y) {
      Direction.NORTH
    } else {
      Direction.SOUTH
    }
  }

  def getNeighbors:Iterable[Point] = {
    Direction.values.map(this.move _)
  }

  def getDiagonals:Iterable[Point] = {
    import Direction._
    val dirs = List((NORTH,EAST),(NORTH,WEST),(SOUTH,EAST),(SOUTH,WEST))
    dirs.map { case (a,b) => this.move(a).move(b) }
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

class World(val size: Int = 8) {
  private[this] val (minRS, maxRS) = (4, 10)
  private[this] val (minCL, maxCL) = (4, 10)

  val dimension = 1 << size
  private[this] val rand = new Random()
  private[this] val m = Array.ofDim[Tile](dimension, dimension)
  private[this] val entitiesByTile = Array.ofDim[ArrayList[Entity]](dimension, dimension)
  private[this] val entities = new ArrayList[Entity]()

  def getTile(p: Point): Tile = if (!checkDimensions(p)) Tile.UNKNOWN else m(p.x)(p.y)
  private[this] def getSprites(p: Point): ArrayList[Entity] = if (!checkDimensions(p)) new ArrayList[Entity]() else entitiesByTile(p.x)(p.y)

  def addPlayer(player: Player) {
    add(player)
  }

  private[this] def add(entity: Entity) {
    entities.add(entity)
    insertEntity(entity.getPos, entity)
  }

  private[this] def remove(entity: Entity) {
    entities.remove(entity)
    removeEntity(entity.getPos, entity)
  }

  private[this] def insertEntity(p: Point, entity: Entity) {
    getSprites(p).add(entity)
  }

  private[this] def removeEntity(p: Point, entity: Entity) {
    getSprites(p).remove(entity)
  }

  def tick() {
    for (i <- 0 until dimension * dimension / 64) {
      val p = Point.nextPoint(rand, dimension, dimension)
      getTile(p).tick(this, p)
    }

    for (e <- entities) {
      e.tick()
      val s = e.getPos

      if (e.removed) {
        remove(e)
      } else {
        val t = e.getPos
        if (s != t) {
          removeEntity(s, e)
          insertEntity(t, e)
        }
      }
    }
  }

  def renderBackground(screen: Screen, xScroll: Int, yScroll: Int) {
    val p = Point.toPoint(xScroll, yScroll)
    val sz = Point.toPoint(screen.w + 15, screen.h + 15)
    screen.offset = (xScroll, yScroll)
    for (y <- p.y to sz.y + p.y; x <- p.x to sz.x + p.x) {
      val np = Point(x, y)
      getTile(np).render(screen, this, np)
    }
    screen.offset = (0, 0)
  }

  def renderSprites(screen: Screen, xScroll: Int, yScroll: Int) {
    val p = Point.toPoint(xScroll, yScroll)
    val sz = Point.toPoint(screen.w + 15, screen.h + 15)

    screen.offset = (xScroll, yScroll)
    for (y <- p.y to sz.y + p.y) {
      val sprites = for (x <- p.x to sz.x + p.x) yield {
        getSprites(Point(x, y))
      }
      sprites.flatten.sortBy(_.y).foreach(_.render(screen))
    }
    screen.offset = (0, 0)
  }

  def generate() {
    for (i <- 0 until dimension; j <- 0 until dimension) {
      m(i)(j) = Tile.EMPTY
      entitiesByTile(i)(j) = new ArrayList[Entity]()
    }

    createRoom(Point(dimension >> 1, dimension >> 1), 1000)
  }

  private[this] def createRoom(c: Point, attempts: Int): Int = {
    if (attempts <= 0) {
      return attempts
    }
    var localAttempts = attempts - 1

    // Generate room with borders on lines bx,by,tx,ty
    val (w, h) = (nextInt(minRS, maxRS), nextInt(minRS, maxRS))
    val (bx, by) = (c.x - w / 2, c.y - h / 2)
    val (tx, ty) = (bx + w, by + h)

    if (!checkDimensions(Point(bx, by)) || !checkDimensions(Point(tx, ty))) {
      return localAttempts
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
    var buildCorridor = true

    while (buildCorridor && localAttempts > 0) {
      localAttempts -= 1

      var d = dirs(rand.nextInt(dirs.size))
      dirs = dirs.filter(_ == d)

      var p = d match {
        case Direction.NORTH => Point(nextInt(bx + 1, tx - 1), ty)
        case Direction.SOUTH => Point(nextInt(bx + 1, tx - 1), by)
        case Direction.EAST => Point(bx, nextInt(by + 1, ty - 1))
        case Direction.WEST => Point(tx, nextInt(by + 1, ty - 1))
      }

      val len = nextInt(minCL, maxCL)
      for (l <- 0 until len) {
        if (checkDimensions(p)) {
          m(p.x)(p.y) = Tile.FLOOR
          p.getNeighbors.foreach(setToWallIfEmpty _)
          p.getDiagonals.foreach(setToWallIfEmpty _)
          buildCorridor = false
        } else {
          buildCorridor = true
        }
        p = p.move(d)
      }

      if (!buildCorridor) {
        val remAttempts = createRoom(p, attempts - 1)
        if (remAttempts == localAttempts) {
          buildCorridor = true
        }
        localAttempts = remAttempts
      }
    }

    localAttempts
  }

  private[this] def nextInt(min: Int, max: Int): Int = rand.nextInt(max - min) + min

  private[this] def setToWallIfEmpty(p: Point) {
    if (checkDimensions(p) && m(p.x)(p.y) == Tile.EMPTY) {
      m(p.x)(p.y) = Tile.WALL
    }
  }

  private[this] def checkDimensions(p: Point): Boolean = (p.x >= 0 && p.x < dimension && p.y >= 0 && p.y < dimension)
}
