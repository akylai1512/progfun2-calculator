package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      b()*b()-4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
  c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal{
    val d = delta()
    val rootd= math.sqrt(d)
    if (d<0)
      Set.empty
    else
      Set((rootd-b())/2*a(),-(b()+ rootd)/2*a())
}
  }
