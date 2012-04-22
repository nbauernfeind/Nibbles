package com.nefariouszhen.ld23.gen

import tile.Tile
import util.Random
import com.nefariouszhen.ld23.graphics.Screen
import java.util.ArrayList
import scala.collection.JavaConversions._
import com.nefariouszhen.ld23.entity.{Enemy, Player, Entity}

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

  def distanceTo(p: Point): Double = math.sqrt(List(x - p.x, y - p.y).map(x => x * x).sum)

  def getNeighbors: Iterable[Point] = {
    Direction.values.map(this.move _)
  }

  def getDiagonals: Iterable[Point] = {
    import Direction._
    val dirs = List((NORTH, EAST), (NORTH, WEST), (SOUTH, EAST), (SOUTH, WEST))
    dirs.map {
      case (a, b) => this.move(a).move(b)
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

object World {
  val MD_SEEN_BIT = 0x1
}

class World(val size: Int = 8) {
  private[this] val (minRS, maxRS) = (4, 10)
  private[this] val (minCL, maxCL) = (4, 10)
  private[this] val monsterDensity = 2

  val dimension = 1 << size
  private[this] val rand = new Random()
  private[this] val m = Array.ofDim[Tile](dimension, dimension)
  private[this] val md = Array.ofDim[Int](dimension * 2, dimension * 2)
  private[this] val entitiesByTile = Array.ofDim[ArrayList[Entity]](dimension, dimension)
  private[this] val entities = new ArrayList[Entity]()

  def getTile(p: Point): Tile = if (!checkDimensions(p)) Tile.UNKNOWN else m(p.x)(p.y)
  private[this] def getSprites(p: Point): ArrayList[Entity] = if (!checkDimensions(p)) new ArrayList[Entity]() else entitiesByTile(p.x)(p.y)

  private[this] var player: Player = null
  def getPlayer = player

  def add(entity: Entity) {
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
    trySpawn(1)

    for (i <- 0 until dimension * dimension / 64) {
      val p = Point.nextPoint(rand, dimension, dimension)
      getTile(p).tick(this, p)
    }

    var i = 0
    while (i < entities.size) {
      val e = entities.get(i)
      val s = e.getPos
      e.tick()

      if (e.removed) {
        entities.remove(i)
        i -= 1
        removeEntity(s, e)
      } else {
        val t = e.getPos
        if (s != t) {
          removeEntity(s, e)
          insertEntity(t, e)
        }
      }

      i += 1
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

  def renderFogOfWar(screen: Screen, player: Player, xScroll: Int, yScroll: Int) {
    val p = Point.toPoint(xScroll, yScroll)
    val sz = Point.toPoint(screen.w + 16, screen.h + 16)

    screen.offset = (xScroll, yScroll)
    val (ppx, ppy) = (player.x - player.x % 8 + 4, player.y - player.y % 8 + 4)
    for (y <- 16 * p.y to 16 * (sz.y + p.y) by 8; x <- 16 * p.x to 16 * (sz.x + p.x) by 8) {
      val dd = List(ppx - x - 4, ppy - y - 4).map(x => x * x).sum
      if (x >= 0 && x < 16 * dimension && y >= 0 && y < 16 * dimension) {
        val percent = if (dd <= player.sightR2) {
          md(x >> 3)(y >> 3) |= World.MD_SEEN_BIT
          1.1
        } else if ((md(x >> 3)(y >> 3) & World.MD_SEEN_BIT) == 0) {
          0.0
        } else {
          0.75
        }

        if (percent < 1.0) {
          screen.darken(x, y, percent)
        }
      }
    }
    screen.offset = (0, 0)
  }

  def trySpawn(cnt: Int) {
    for (i <- 0 until cnt) {
      val lvl = rand.nextInt(4) - 1 + player.lvl
      val e = new Enemy(this, lvl)
      if (findStartPos(e)) {
        add(e)
      }
    }
  }

  private[this] def findStartPos(e: Enemy): Boolean = {
    val p = Point.nextPoint(rand, dimension, dimension)
    val (x, y) = (16 * p.x + 8, 16 * p.y + 8)

    val (xd, yd) = (player.x - x, player.y - y)
    if (xd * xd + yd * yd < 80 * 80)
      return false

    val densityR = monsterDensity * 16
    if (getEntities(x - densityR, y - densityR, x + densityR, y + densityR).size > 0)
      return false

    if (getTile(p).mayPass(this, p, e)) {
      e.x = x
      e.y = y
      return true
    }

    false
  }

  def getEntities(x0: Int, y0: Int, x1: Int, y1: Int): Iterable[Entity] = {
    import Direction._
    val p0 = Point.toPoint(x0, y0).move(NORTH).move(EAST)
    val p1 = Point.toPoint(x1, y1).move(SOUTH).move(WEST)

    val entities = for (y <- p0.y to p1.y; x <- p0.x to p1.x) yield {
      getSprites(Point(x, y)).filter(_.intersects(x0, y0, x1, y1))
    }

    entities.flatten
  }

  def generate(newPlayer: Player) {
    for (i <- 0 until dimension; j <- 0 until dimension) {
      m(i)(j) = Tile.EMPTY
      entitiesByTile(i)(j) = new ArrayList[Entity]()
    }

    for (i <- 0 until 2 * dimension; j <- 0 until 2 * dimension) {
      md(i)(j) = 0
    }

    entities.toList.foreach(remove _)

    createRoom(Point(dimension >> 1, dimension >> 1), 1000)

    player = newPlayer
    player.x = dimension / 2 * 16 + 4
    player.y = dimension / 2 * 16 + 4
    add(player)
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
