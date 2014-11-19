import breeze.linalg._
import breeze.stats.distributions._
import breeze.numerics._
import breeze.stats.mean

import scala.util.Random

trait LinkFunction {
  /* e.g. log */
  def link(mu: Double): Double
  /* e.g. exp */
  def ilink(mu: Double): Double
  /* e.g. 1/mu */
  def dlink(mu: Double): Double
}

class IdentityLink extends LinkFunction {
  def link(mu: Double): Double = mu
  def ilink(mu: Double): Double = mu
  def dlink(mu: Double): Double = 1
}

class LogLink extends LinkFunction {
  def link(mu: Double): Double = log(mu)
  def ilink(mu: Double): Double = exp(mu)
  def dlink(mu: Double): Double = 1.0 / mu
}

