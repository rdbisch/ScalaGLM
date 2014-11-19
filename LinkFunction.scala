package org.rbisd.glm;
/** Reperesents the link function of glm theory
  * 
  * Function can be anything as long as it is monotonic
  *  and invertible across the entire domain 
  *
  * In literature,
  * link is usually denoted g
  * ilink is the inverse of g, 
  * dlink is the derivative of g.
  */
trait LinkFunction {
  def link(mu: Double): Double
  def ilink(mu: Double): Double
  def dlink(mu: Double): Double
}

class IdentityLink extends LinkFunction {
  def link(mu: Double): Double = mu
  def ilink(mu: Double): Double = mu
  def dlink(mu: Double): Double = 1
}

class LogLink extends LinkFunction {
  def link(mu: Double): Double = scala.math.log(mu)
  def ilink(mu: Double): Double = scala.math.exp(mu)
  def dlink(mu: Double): Double = 1.0 / mu
}

