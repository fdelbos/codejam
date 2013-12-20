// 
// tic_tac_toe.scala
// 
// Created by Frederic DELBOS - fred.delbos@gmail.com on Dec 20 2013.
// 

import scala.io.Source

def c4(g: (Int) => Char) = {
  (g(0) /: 0.until(4))((p, idx) => (p, g(idx)) match {
    case ('.', _) => '.'
    case (_, '.') => '.'
    case ('?', 'T')  => 'T'
    case (_, 'T') if p != '.' => p
    case ('T', c) => c
    case (_, `p`) => p
    case _ => '!'})
}

def check(grd: IndexedSeq[String]) = {
  var finish = true

  def found(c: Char, g: (Int) => Char):Char = c match {
    case 'X' => c
    case 'O' => c
    case '.' => {finish = false; c4(g)}
    case _ => c4(g)}

  val fs = List(
    (() => ('?' /: 0.until(4))((c, y) => found(c, ((x) => grd(y)(x))))),
    (() => ('?' /: 0.until(4))((c, x) => found(c, ((y) => grd(y)(x))))),
    (() => found('?', ((i) => grd(i)(i)))),
    (() => found('?', ((i) => grd(-i + 3)(i)))))

  ('?' /: fs)((c, f) => c match {
    case _ if c == 'X'|| c == 'O' => c
    case _ => f()}) match {
    case 'X' => "X won"
    case 'O' => "O won"
    case _ if finish == true => "Draw"
    case _ => "Game has not completed"}
}

argv.toList match {
  case h :: l => {
    val str = Source.fromFile(h).mkString.split(Array(' ', '\n', '\r', '\t'))
    (1 /: 1.to(str(0).toInt))((i, pos) => {
      println(s"Case #$pos: ${check(str.slice(i, i + 4))}")
      i + 5
    })
  }
  case _ => println("missing input file")
}
