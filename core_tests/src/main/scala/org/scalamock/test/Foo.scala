package org.scalamock.test

trait Foo1[A] {
  def bar[B](a: A): A
}

trait Foo2[A] {
  def bar[B](a: A, b: B): (A, B)
}