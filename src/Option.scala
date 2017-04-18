/**
  * Created by nicos on 4/17/17.
  */
import org.apache.commons.math3.distribution

class Option {
  var ticker = ""
  var strikePrice : Double = -1
  var daysToExp : Int = -1
  var contracts : Int = 0
  var volatility : Double = -1
  var isCall = true

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

}
