/**
  * Created by nicos on 4/17/17.
  */
class OptionsDSL {

  val option = new Option

  object Option {

    def put (ticker: String) : Option = {
      val op = new Option
      op.isCall = false
      op.ticker = ticker
      return op
    }

    def call (ticker: String) : Option = {
      val op = new Option
      op setIsCall true
      op.ticker = ticker
      return op
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
  }

  object Stats {
    def of (op : Option) : Unit = {
      of(new Spread and op)
    }

    def of (sp : Spread) : Unit = {
      println("Max gain: " + sp.maxgain() + " at underlying price: " + sp.maxXPrice)
      println("Max loss: " + sp.maxloss() + " at underlying price: " + sp.maxXPrice)
    }
  }

  def maxloss (sp : Spread) : Double = {
    sp maxloss
  }

  def maxgain (sp : Spread) : Double = {
    sp maxgain
  }

}
