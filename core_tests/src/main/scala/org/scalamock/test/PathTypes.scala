package org.scalamock.test

trait PathTypes {
  trait Embedded {}
  
  def referencesEmbedded(): Embedded
}

object PathTypes {
  val x = org.scalamock.Implement.implement[PathTypes]
}