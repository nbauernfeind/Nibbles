package com.nefariouszhen.ld23.screen

import com.nefariouszhen.ld23.{InputHandler, Game}
import com.nefariouszhen.ld23.graphics.{Font, Screen}

abstract class TextMenu(parent: Menu, game: Game, input: InputHandler) extends Menu {
  def msgs: List[String]

  def tick() {
    if (input.attack.clicked || input.menu.clicked)
      game.setMenu(Some(parent))
  }

  def render(screen: Screen) {
    screen.clear()

    var x = 1
    for (i <- msgs) {
      Font.draw(i, screen, 4 * 8, x * 8)
      x += 1
    }
  }
}

class InstructionsMenu(parent: Menu, game: Game, input: InputHandler) extends TextMenu(parent, game, input) {
  // Yes, that is a joke thanking Notch for his ld22 source.
  val msgs = List(
    "  HOW TO PLAY:",
    "",
    "",
    "Move with the arrow keys.",
    "Press C to attack.",
    "Press X to open the menu.",
    "",
    "Defeat enemies to gain",
    "experience and level.",
    "",
    "Mix and match Nibbles",
    "programs for top notch",
    "quality."
  )
}

class AboutMenu(parent: Menu, game: Game, input: InputHandler) extends TextMenu(parent, game, input) {
  val msgs = List(
    "  About Nibble",
    "",
    "",
    "Nibble was made by",
    "Nathaniel Bauernfeind",
    "aka Nefarious Zhen",
    "",
    "",
    "",
    "It was heavily inspired by",
    "Minicraft."
  )
}
