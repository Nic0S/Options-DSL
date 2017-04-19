/**
  * Created by nicos on 4/17/17.
  */
import org.apache.commons.math3.distribution.NormalDistribution

class Option {
  var ticker = ""
  var strikePrice : Double = -1
  var daysToExp : Int = -1
  var contracts : Int = 0
  var volatility : Double = -1
  var isCall = true

  val interestRate = 0.01

  def strike (p : Double) : Option = {
    strikePrice = p
    return this
  }

  def expiration (d : Int) : Option = {
    daysToExp = d
    return this
  }

  def ticker (t: String) : Option = {
    ticker = t
    return this
  }

  def contracts (c : Int) : Option = {
    contracts = c
    return this
  }

  def volatility (v : Double) : Option = {
    volatility = v
    return this
  }

  def setIsCall (c : Boolean) : Option = {
    isCall = c
    return this
  }

  def calculatePrice (underlyingPrice : Double) : Double = {
    val dist = new NormalDistribution()
    val t = daysToExp / 365f

    var strike : Double = -1
    if (isCall) {
      strike = strikePrice
    } else {
      strike = underlyingPrice + (underlyingPrice - strikePrice)
    }

    val d1 = (Math.log(underlyingPrice / strike) + (interestRate + volatility * volatility / 2) * t) /
      (volatility * Math.sqrt(t))
    val d2 = d1 - volatility * Math.sqrt(t)

    contracts * (underlyingPrice * dist.cumulativeProbability(d1) - strike * Math.exp(-interestRate * t) *
      dist.cumulativeProbability(d2))
  }

}
