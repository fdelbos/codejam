// 
// lawnmower.scala
// 
// Created by Frederic DELBOS - fred.delbos@gmail.com on Dec 20 2013.
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// 

import scala.io.Source

def check(x: Int, y: Int, lawn: Array[Array[Int]]) =
  0.until(y).forall((i) =>
    0.until(x).forall((j) => 
      0.until(y).forall((k) => lawn(k)(j) <= lawn(i)(j)) match {
        case false => 0.until(x).forall((m) =>
          lawn(i)(m) <= lawn(i)(j))
        case _ => true}))

argv.toList match {
  case h :: l => {
    val tab = Source.fromFile(h).mkString.split(Array('\n'))
    1.to(tab(0).toInt).foldLeft(1)((i, pos) => {
      val Array(y, x) = tab(i).split(" ").map(_.toInt)
      
      check(x, y, tab.slice(i + 1, i + 1 + y).map(_.split(" ").map(_.toInt))) match {
        case true => println(s"Case #$pos: YES")
        case _ => println(s"Case #$pos: NO")
      }
      1 + i + y})}
  case _ => println("missing input file")
}
