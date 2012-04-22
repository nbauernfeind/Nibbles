package com.nefariouszhen.ld23.screen

import com.nefariouszhen.ld23.{Game, InputHandler}
import com.nefariouszhen.ld23.graphics.{Font, Screen}

trait MenuItem {
  def renderInventory(screen: Screen, x0: Int, y0: Int)
}

abstract class Menu {
  def tick()
  def render(screen: Screen)

  def renderItemList(screen: Screen, x0: Int, y0: Int, x1: Int, y1: Int, items: List[MenuItem], selected: Int) {
    for (i <- 0 until items.length) {
      items(i).renderInventory(screen, (1 + x0) * 8, (i + 1 + y0) * 8)
    }

    val yy = selected + 1 + y0
    for ((msg,xo) <- List((">", 0),("<", x1-x0))) {
      Font.draw(">", screen, (x0 + xo)*8, yy*8, 0)
    }
  }
}
