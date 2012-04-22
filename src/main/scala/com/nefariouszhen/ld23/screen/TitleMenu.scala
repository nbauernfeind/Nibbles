package com.nefariouszhen.ld23.screen

import com.nefariouszhen.ld23.{InputHandler, Game}
import com.nefariouszhen.ld23.graphics.{Font, Screen}
import com.nefariouszhen.ld23.entity.genome.SpriteInfo

class TitleMenu(game: Game, input: InputHandler) extends Menu {
  var selected = 0
  val options = List("Start game", "How to play", "About")

  def tick() {
    if (input.up.clicked) selected -= 1
    if (input.down.clicked) selected += 1

    if (selected < 0) selected += options.length
    if (selected >= options.length) selected -= options.length

    if (input.attack.clicked || input.menu.clicked) {
      selected match {
        case 0 =>
          game.resetGame()
          game.setMenu(None)
        case 1 => game.setMenu(Some(new InstructionsMenu(this, game, input)))
        case 2 => game.setMenu(Some(new AboutMenu(this, game, input)))
      }
    }
  }

  def render(screen: Screen) {
    screen.fill(0)

    val titleSprite = SpriteInfo(0, 5, 7, 2)
    screen.renderSprite((screen.w - titleSprite.w * 8) / 2, 32, titleSprite)

    for (i <- 0 until options.length) {
      var c = 0xcccccc
      var msg = options(i)
      if (i == selected) {
        msg = "> " + msg + " <"
        c = 0xffffff
      }

      Font.draw(msg, screen, (screen.w - msg.length() * 8) / 2, (8 + i) * 8, c)
    }
  }
}
