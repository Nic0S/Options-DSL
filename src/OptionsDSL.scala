/**
  * Created by nicos on 4/17/17.
  */
class OptionsDSL {

  val formatter = java.text.NumberFormat.getCurrencyInstance

  val option = new Option

  object Option {

    def put (ticker: String) : Option = {
      val op = new Option
      op.isCall = false
      op.ticker = ticker
       op
    }

    def call (ticker: String) : Option = {
      val op = new Option
      op.isCall = true
      op.ticker = ticker
       op
    }

    def put (p : Double) : Option = {
      val op = new Option
      op.isCall = false
      op.strikePrice = p
      op
    }

    def call (p : Double) : Option = {
      val op = new Option
      op.isCall = true
      op.strikePrice = p
      op
    }
  }

  object Spread {
    def of (op : Option) : Spread = {
      val spread = new Spread
      spread and op
    }

    def short (count : Integer) : SpreadBuilder = {
      val sb = new SpreadBuilder
      sb.count = - count
      sb
    }

    def long (count : Integer) : SpreadBuilder = {
      val sb = new SpreadBuilder
      sb.count = count
      sb
    }
  }

  object Stats {
    def of (op : Option) : Unit = {
      of(new Spread and op)
    }

    def of (sp : Spread) : Unit = {
      val maxGain  =formatter.format(sp.maxgain())
      var maxGainPrice = ""
      if (sp.maxXPrice == Double.MaxValue) {
        maxGainPrice = "$∞"
      } else {
        maxGainPrice = formatter.format(sp.maxXPrice)
      }

      val maxloss = formatter.format(sp.maxloss())
      var maxLossPrice = ""
      if (sp.maxXPrice == Double.MaxValue) {
        maxLossPrice = "$∞"
      } else {
        maxLossPrice = formatter.format(sp.maxXPrice)
      }

      println("Max gain: " + maxGain + " at underlying price: " + maxGainPrice)
      println("Max loss: " + maxloss + " at underlying price: " + maxLossPrice)
    }
  }

  object PL {
    def of (sp : Spread) : Unit = {
      sp print "PL"
    }
  }

  object Value {
    def of (sp : Spread) : Unit = {
      sp print "Value"
    }
  }

  def maxloss (sp : Spread) : Double = {
    sp maxloss
  }

  def maxgain (sp : Spread) : Double = {
    sp maxgain
  }

}
