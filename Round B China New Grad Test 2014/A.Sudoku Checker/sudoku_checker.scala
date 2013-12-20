// 
// sudoku_checker.scala
// 
// Created by Frederic DELBOS - fred.delbos@gmail.com on Dec 14 2013.
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// 

import scala.io.Source

def line(n: Int, move:(Int) => Int, i: Int = 0, res: Long = 0): Boolean = i match {
  case _ if i == n*n => res == (1.toLong << n * n) - 1
  case _ => move(i) match {
    case x if x > n*n || x < 1 => false
    case x => line(n, move, i + 1, res | (1.toLong << x - 1))}
}

def rows(n: Int, grd: IndexedSeq[Int]) =
  0.until(n*n*n*n, n*n).forall((x) => line(n, (y: Int) => grd(y + x)))

def columns(n: Int, grd: IndexedSeq[Int]) =
  0.until(n*n).forall((x) => line(n, (y: Int) => grd(y * n*n + x)))

def matrices(n: Int, grd: IndexedSeq[Int]) =
  0.until(n*n, n).forall((x) =>
    x.until(n*n*n*n, n*n*n).forall((y) =>
      line(n, (z: Int) => grd(z / n*n*n + z % n + y))))

def check(n: Int, grd: IndexedSeq[Int]) =
  rows(n, grd) && columns(n, grd) && matrices(n, grd)

argv.toList match {
  case h :: l => {
    val tab = Source.fromFile(h).mkString.split(Array(' ', '\n', '\r', '\t'))
    1.to(tab(0).toInt).foldLeft(1)((i, pos) => {
      val n = tab(i).toInt
      check(n, (i + 1).to(i + n*n*n*n).map(tab(_).toInt)) match {
        case true => println(s"Case #$pos: Yes")
        case _ => println(s"Case #$pos: No")}
      n * n * n * n + 1 + i})}
  case _ => println("missing input file")
}
