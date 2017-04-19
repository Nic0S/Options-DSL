import org.apache.commons.math3.distribution.NormalDistribution

class Option {
  var ticker = ""
  var strikePrice : Double = -1
  var daysToExp : Int = -1
  var contracts : Int = 1
  var volatility : Double = -1
  var isCall = true
  var costBasis : Double = 0

  var interestRate = 0.01

  def strike (p : Double) : Option = {
    val modified = copy()
    modified.strikePrice = p
    modified
  }

  def expiration (d : Int) : Option = {
    val modified = copy()
    modified.daysToExp = d
    modified
  }

  def ticker (t: String) : Option = {
    val modified = copy()
    modified.ticker = t
    modified
  }

  def contracts (c : Int) : Option = {
    val modified = copy()
    modified.contracts = c
    modified
  }

  def volatility (v : Double) : Option = {
    val modified = copy()
    modified.volatility = v
    modified
  }

  def setIsCall (c : Boolean) : Option = {
    val modified = copy()
    modified.isCall = c
    modified
  }

  def irate (i : Double) : Option = {
    val modified = copy()
    modified.interestRate = i
    modified
  }

  def cost (c : Double) : Option = {
    val modified = copy()
    modified.costBasis = c
    modified
  }

  def copy () : Option = {
    val copy = new Option
    copy.ticker = ticker
    copy.strikePrice = strikePrice
    copy.daysToExp = daysToExp
    copy.contracts = contracts
    copy.volatility = volatility
    copy.isCall = isCall
    copy.interestRate = interestRate
    copy.costBasis = costBasis
  }

  def calculatePrice (underlyingPrice : Double) : Double = {
    require(strikePrice != -1, "Strike price is not set on option: " + toString())
    require(daysToExp != -1, "Days to expiration is not set on option: " + toString())
    require(volatility != -1, "Volatility is not set on option: " + toString())

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

  def calculatePL (underlyingPrice : Double) : Double = {
    require(costBasis != 0, "Cost basis not set on option: " + toString())

    val price = calculatePrice(underlyingPrice)
    price - costBasis
  }

  override def toString() : String = {
    var typeText = ""
    if (isCall) {
      typeText = "Call"
    } else {
      typeText = "Put"
    }
    contracts + " " + ticker + " " + typeText + " | Strike: " + strikePrice + " | expiration: " + daysToExp
  }

}
