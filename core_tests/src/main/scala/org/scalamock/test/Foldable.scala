package org.scalamock.test

trait Foldable[+A] {

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1
}