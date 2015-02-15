package org.scalamock.test

import org.scalatest.FreeSpec

class ImplementTest extends FreeSpec {
  
  "Implement should" - {
    "be able to implement polymorphic traits" in {
      val f0 = org.scalamock.Implement.implement[Foo0]
      f0.bar(42)
      val f1 = org.scalamock.Implement.implement[Foo1[Int]]
      f1.bar(42)
      val f2 = org.scalamock.Implement.implement[Foo2[Int]]
      f2.bar(42, 1.23)
    }
  }
}
