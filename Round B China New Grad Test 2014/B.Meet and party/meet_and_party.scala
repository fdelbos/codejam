// 
// meet_and_party.scala
// 
// Created by Frederic DELBOS - fred.delbos@gmail.com on Dec 19 2013.
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// 

import scala.io.Source

case class Ne(x1: Int, y1: Int, x2: Int, y2: Int)
case class Cdt(x: Int, y: Int, dist: Long)

def distance(tx: Int, ty: Int, nes: IndexedSeq[Ne]): Long = 
  (0 /: nes)((d, ne) =>
    (d /: ne.x1.to(ne.x2))((d, x) =>
      (d /: ne.y1.to(ne.y2))((d, y) =>
        d + (tx - x).abs + (ty - y).abs)))

def closest(nes: IndexedSeq[Ne]) =
  (Cdt(nes(0).x1, nes(0).y1, Long.MaxValue) /: nes)((c, ne) =>
    (c /: ne.x1.to(ne.x2))((c, x) =>
      (c /: ne.y1.to(ne.y2))((c, y) => distance(x, y, nes) match {
        case d if d < c.dist => Cdt(x, y, d)
        case d if d == c.dist && x < c.x => Cdt(x, y, d)
        case d if d == c.dist && x == c.x && y < c.y => Cdt(x, y, d)
        case _ => c})))

argv.toList match {
  case h :: l => {
    val str = Source.fromFile(h).mkString.split(Array(' ', '\n', '\r', '\t'))
    (1 /: 1.to(str(0).toInt))((i, pos) => {
      val n = str(i).toInt
      val nes  = (i + 1).until(i + 1 + n * 4, 4).map((x) =>
        Ne(str(x).toInt, str(x + 1).toInt, str(x + 2).toInt, str(x + 3).toInt))
      val c = closest(nes)
      println(s"Case #$pos: ${c.x} ${c.y} ${c.dist}") 
      n * 4 + i + 1})}
  case _ => println("missing input file")
}
