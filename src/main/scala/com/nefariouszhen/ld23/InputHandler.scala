package com.nefariouszhen.ld23

import java.awt.event.{KeyEvent, KeyListener}
import collection.mutable.ArrayBuffer

class InputHandler(game: Game) extends KeyListener {
  game.addKeyListener(this)

  class Key {
    var presses = 0
    var absorbs = 0
    var down = false
    var clicked = false

    keys += this

    def toggle(pressed: Boolean) {
      if (pressed != down)
        down = pressed
      if (pressed)
        presses += 1
    }

    def tick() {
      if (absorbs < presses) {
        absorbs += 1
        clicked = true
      } else {
        clicked = false
      }
    }
  }

  val keys = ArrayBuffer[Key]()

  val up = new Key
  val down = new Key
  val left = new Key
  val right = new Key
  val regen = new Key
  val attack = new Key
  val menu = new Key

  def releaseAll() {
    keys.foreach(_.down = false)
  }

  def tick() {
    keys.foreach(_.tick())
  }

  def toggle(e: KeyEvent, pressed: Boolean): Unit = e.getKeyCode match {
    case KeyEvent.VK_W => up.toggle(pressed)
    case KeyEvent.VK_S => down.toggle(pressed)
    case KeyEvent.VK_D => right.toggle(pressed)
    case KeyEvent.VK_A => left.toggle(pressed)

    case KeyEvent.VK_UP => up.toggle(pressed)
    case KeyEvent.VK_RIGHT => right.toggle(pressed)
    case KeyEvent.VK_DOWN => down.toggle(pressed)
    case KeyEvent.VK_LEFT => left.toggle(pressed)

    case KeyEvent.VK_NUMPAD8 => up.toggle(pressed)
    case KeyEvent.VK_NUMPAD6 => right.toggle(pressed)
    case KeyEvent.VK_NUMPAD2 => down.toggle(pressed)
    case KeyEvent.VK_NUMPAD4 => left.toggle(pressed)

    case KeyEvent.VK_R => regen.toggle(pressed)

    case KeyEvent.VK_SPACE => attack.toggle(pressed)
    case KeyEvent.VK_CONTROL => attack.toggle(pressed)
    case KeyEvent.VK_NUMPAD0 => attack.toggle(pressed)
    case KeyEvent.VK_INSERT => attack.toggle(pressed)
    case KeyEvent.VK_C => attack.toggle(pressed)

    case KeyEvent.VK_TAB => menu.toggle(pressed)
    case KeyEvent.VK_ALT => menu.toggle(pressed)
    case KeyEvent.VK_ALT_GRAPH => menu.toggle(pressed)
    case KeyEvent.VK_ENTER => menu.toggle(pressed)
    case KeyEvent.VK_X => menu.toggle(pressed)

    case _ =>
  }

  def keyPressed(e: KeyEvent): Unit = toggle(e, true)
  def keyReleased(e: KeyEvent): Unit = toggle(e, false)
  def keyTyped(e: KeyEvent) {}
}
