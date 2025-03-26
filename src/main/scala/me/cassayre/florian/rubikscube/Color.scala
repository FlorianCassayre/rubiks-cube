package me.cassayre.florian.rubikscube

enum Color {
  case Red
  case Orange
  case Blue
  case Green
  case White
  case Yellow

  def toChar: Char = this match
    case Color.Red => 'R'
    case Color.Orange => 'O'
    case Color.Blue => 'B'
    case Color.Green => 'G'
    case Color.White => 'W'
    case Color.Yellow => 'Y'

  def toAnsiColor: String = this match
    case Color.Red => "\u001b[31m"
    case Color.Orange => "\u001b[33m"
    case Color.Blue => "\u001b[34m"
    case Color.Green => "\u001b[32m"
    case Color.White => "\u001b[37m"
    case Color.Yellow => "\u001b[38;5;11m"

  def toAnsiColoredChar: String = s"$toAnsiColor$toChar\u001b[0m"
}
