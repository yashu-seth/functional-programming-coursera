package calculator

import scala.math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    if (delta() < 0) Signal(Set())
    else if (delta() == 0) Signal(Set( -b() / (2 * a()) ))
    else {
        val delta_sqrt = sqrt(delta())
        Signal(Set( (-b() + delta_sqrt) / (2 * a()) , (-b() - delta_sqrt) / (2 * a()) ))
    }
  }
}
