import org.apache.commons.math3.distribution.NormalDistribution

class Option {
  var ticker = ""
  var strikePrice : Double = -1
  var daysToExp : Int = -1
  var contracts : Int = 1
  var volatility : Double = -1
  var isCall = true
  var costBasis : Double = 0
  var origUnderlyingPrice : Double = -1;

  var interestRate = 0.01

  // Returns a copy of this with modified strike
  def strike (p : Double) : Option = {
    val modified = copy()
    modified.strikePrice = p
    modified
  }

  // Returns copy with t subtracted from the expiry
  def time (t : Integer) : Option = {
    val modified = copy()
    modified.daysToExp -= t
    modified
  }

  // Returns a copy with the given expiration.
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

  // Returns a copy with a modified number of contracts (contracts are for 100 shares)
  def contracts (c : Int) : Option = {
    val modified = copy()
    modified.contracts = c
    modified
  }

  // Returns a copy with modified volatility.
  def volatility (v : Double) : Option = {
    val modified = copy()
    modified.volatility = v
    modified
  }

  // Changes this option from a call to a put or the other way around
  def setIsCall (c : Boolean) : Option = {
    val modified = copy()
    modified.isCall = c
    modified
  }

  // Change the risk free interest rate for calculations.
  def irate (i : Double) : Option = {
    val modified = copy()
    modified.interestRate = i
    modified
  }

  // Change the cost basis for this option
  def cost (c : Double) : Option = {
    val modified = copy()
    modified.costBasis = c
    modified
  }

  // Return a deep copy of this option
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
    copy.origUnderlyingPrice = origUnderlyingPrice
    copy
  }

  def fillCost (underlyingPrice : Double) : Option = {
    val modified = copy()
    modified.costBasis = modified calculateValue underlyingPrice
    modified.origUnderlyingPrice = underlyingPrice
    modified
  }

  // Calculates the current value of this option based on the given underlying price.
  def calculateValue (underlyingPrice : Double) : Double = {
    require(strikePrice != -1, "Strike price is not set on option: " + toString())
    require(daysToExp != -1, "Days to expiration is not set on option: " + toString())
    require(volatility != -1, "Volatility is not set on option: " + toString())

    val dist = new NormalDistribution()
    val t = daysToExp / 365f

    val d1 = (Math.log(underlyingPrice / strikePrice) + (interestRate + volatility * volatility / 2) * t) /
      (volatility * Math.sqrt(t))
    val d2 = d1 - volatility * Math.sqrt(t)

    var price : Double = underlyingPrice * dist.cumulativeProbability(d1) - strikePrice * Math.exp(-interestRate * t) *
      dist.cumulativeProbability(d2)

    // put = strike + call - stock
    if (!isCall) {
      price = strikePrice + price - underlyingPrice
    }

    100 * contracts * price
  }

  def calculatePL (underlyingPrice : Double) : Double = {
    require(costBasis != 0, "Cost basis not set on option: " + toString())

    val price = calculateValue(underlyingPrice)
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
