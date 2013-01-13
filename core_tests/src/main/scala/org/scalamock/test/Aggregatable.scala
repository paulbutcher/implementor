package org.scalamock.test

trait Aggregatable[+A] {

  def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B
}