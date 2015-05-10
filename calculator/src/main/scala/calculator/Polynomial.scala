package calculator
import math.pow
import math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(pow(b(), 2.0) - (4.0 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
        var result = Var(Set.empty[Double])
        if (delta() > 0) {
          result() = Set(
            (-b() + sqrt(delta())) / (2.0 * a()),
            (-b() - sqrt(delta())) / (2.0 * a())
          )
        } 
        result()
    }
  }
}
