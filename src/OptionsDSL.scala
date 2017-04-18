/**
  * Created by nicos on 4/17/17.
  */
class OptionsDSL {

  val option = new Option

  object Option {

    def put (ticker: String) : Option = {
      var op = new Option
      op setIsCall false
      op.ticker = ticker
      return op
    }

    def call (ticker: String) : Option = {
      var op = new Option
      op setIsCall true
      op.ticker = ticker
      return op
    }
  }
}
