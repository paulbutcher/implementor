package org.scalamock.test

trait Foo0 {
  def bar[B](a: B): B
}

trait Foo1[A] {
  def bar[B](a: A): A
}

trait Foo2[A] {
  def bar[B](a: A, b: B): (A, B)
}

object FooTests {
  val f0 = org.scalamock.Implement.implement[Foo0]
  val f1 = org.scalamock.Implement.implement[Foo1[Int]]
  val f2 = org.scalamock.Implement.implement[Foo2[Int]]
}
