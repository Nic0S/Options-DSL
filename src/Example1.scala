
object Example1 extends OptionsDSL {
  def main ( args: Array[String]) = {

    // A call with strike $110 purchased 30 days before expiration when the price was $100
    var call = Option call 110 expiration 30 volatility 0.35 fillCost 100

    println(call time 15 calculateValue 100)

    Plot of call min 85 max 130 show()
//
    var put = Option put 90 expiration 30 volatility 0.35 fillCost 100
//
    Stats of put
//
    Plot of put min 85 max 120 show()

    var short_put = put contracts -1 fillCost 100

    Plot of short_put min 85 max 120 show()

    Stats of short_put
  }
}
