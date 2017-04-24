import java.util

class Spread {
  var stepSize : Double = -1
  var start : Double = -1
  var end : Double = -1

  var options = List[Option]()

  var maxXPrice : Double = -1

  val formatter = java.text.NumberFormat.getCurrencyInstance

  def and (op : Option) : Spread = {
    val copy : Spread = this.copy()
    copy.options = copy.options :+ op
    copy
  }

  def short (count : Integer) : SpreadBuilder = {
    val sb = new SpreadBuilder
    sb.spread = this
    sb.count = - count
    sb
  }

  def long (count : Integer) : SpreadBuilder = {
    val sb = new SpreadBuilder
    sb.spread = this
    sb.count = count
    sb
  }

  def copy() : Spread = {
    val copy = new Spread
    for (o : Option <- options) {
      copy.options = copy.options :+ (o copy())
    }
    copy.start = start
    copy.end = end
    copy.stepSize = stepSize
    copy
  }

  def start (p : Double) : Spread = {
    val modified = this copy()
    modified.start = p
    modified
  }

  def end (p : Double) : Spread = {
    val modified = this copy()
    modified.end = p
    modified
  }

  def step (p : Double) : Spread = {
    val modified = this copy()
    modified.stepSize = p
    modified
  }

  def print (metric : String) : Unit = {
    require(options.nonEmpty, "Cannot print data on empty spread")
    require(metric.toLowerCase().equals("value") || metric.toLowerCase().equals("pl"), "Print type must be Value or PL")

    if (start == -1) {
      start = options.head.strikePrice * 0.8
    }
    if (end == -1) {
      end = options.head.strikePrice * 1.2
    }
    if (stepSize == -1) {
      stepSize = (end - start) / 5
    }

    var testPrice : Double = start

    println("Underlying price   " + metric)

    while (testPrice < end) {
      if (metric.toLowerCase().equals("value")) {
        println(formatter.format(testPrice) + "\t\t\t" + formatter.format(calculateValue(testPrice)))
      } else {
        println(formatter.format(testPrice) + "\t\t\t" + formatter.format(calculatePL(testPrice)))
      }

      testPrice += stepSize
    }
  }

  def calculateValue (underlyingPrice : Double) : Double = {
    var totalPrice : Double = 0

    for (o : Option <- options) {
      totalPrice += o calculateValue underlyingPrice
    }

    totalPrice
  }

  def calculatePL (underlyingPrice : Double) : Double = {
    var totalPL : Double = 0

    for (o : Option <- options) {
      totalPL += o calculatePL underlyingPrice
    }

    totalPL
  }

  def totalCost () : Double = {
    var totalCost : Double = 0

    for (o : Option <- options) {
      totalCost += o.costBasis
    }

    totalCost
  }

  def expiration (daysToExp : Int) : Spread = {
    val modified : Spread = new Spread

    for (o : Option <- options) {
      modified.options = modified.options :+ (o expiration daysToExp)
    }

    modified
  }

  def time (daysLater : Int) : Spread = {
    val modified : Spread = new Spread

    for (o : Option <- options) {
      require(o.daysToExp >= daysLater, "Cannot value options post expiration")
      modified.options = modified.options :+ (o expiration (o.daysToExp - daysLater))
    }

    modified
  }

  def volatility (vol : Double) : Spread = {
    val modified : Spread = new Spread

    for (o : Option <- options) {
      modified.options = modified.options :+ (o volatility vol)
    }

    modified
  }

  def irate (i : Double) : Spread = {
    val modified : Spread = new Spread

    for (o : Option <- options) {
      modified.options = modified.options :+ (o irate i)
    }

    modified
  }

  def maxloss () : Double = {
    var maxStrike : Double = 0
    var minStrike = Double.MaxValue

    for (o : Option <- options) {
      if (o.strikePrice > maxStrike) {
        maxStrike = o.strikePrice
      }
      if (o.strikePrice < minStrike) {
        minStrike = o.strikePrice
      }
    }

    var expired : Spread = this expiration 0

    var step : Double = (maxStrike - minStrike) / 100

    var strike : Double = 0

    var maxloss : Double = Double.MinValue

    while (strike < maxStrike) {
      val loss =  - expired.calculatePL(strike)
      if (loss > maxloss) {
        maxloss = loss
        maxXPrice = strike
      }
      strike += step
    }

    var loss : Double = - expired.calculatePL(0)
    if (loss > maxloss) {
      maxloss = loss
      maxXPrice = 0
    }

    loss = - expired.calculatePL(Double.MaxValue)
    if (loss > maxloss) {
      maxloss = loss
      maxXPrice = Double.MaxValue
    }

    maxloss
  }

  def maxgain () : Double = {
    var maxStrike : Double = 0
    var minStrike = Double.MaxValue

    for (o : Option <- options) {
      if (o.strikePrice > maxStrike) {
        maxStrike = o.strikePrice
      }
      if (o.strikePrice < minStrike) {
        minStrike = o.strikePrice
      }
    }

    var expired : Spread = this expiration 0

    var step : Double = (maxStrike - minStrike) / 100

    var strike : Double = 0

    var maxgain : Double = Double.MinValue

    while (strike < maxStrike) {
      val gain = expired.calculatePL(strike)
      if (gain > maxgain) {
        maxgain = gain
        maxXPrice = strike
      }
      strike += step
    }

    var gain : Double = expired.calculatePL(0)
    if (gain > maxgain) {
      maxgain = gain
      maxXPrice = 0
    }

    gain = expired.calculatePL(Double.MaxValue)
    if (gain > maxgain) {
      maxgain = gain
      maxXPrice = Double.MaxValue
    }

    maxgain
  }

  override def toString() : String = {
    var s : String = ""

    for (o : Option <- options) {
      s ++= o.toString() + "\n"
    }

    s
  }
}
