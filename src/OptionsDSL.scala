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
  }

  def maxloss (sp : Spread) : Double = {
    sp maxloss
  }

  def maxgain (sp : Spread) : Double = {
    sp maxgain
  }

}
