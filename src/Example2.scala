object Example2 extends OptionsDSL {
  def main ( args: Array[String]) : Unit = {

    // A call with strike $110 purchased 30 days before expiration when the price was $100
    var call = Option call 110 expiration 30 volatility 0.35 fillCost 100
    var put = Option put 90 expiration 30 volatility 0.35 fillCost 100

    var spread = Spread long 1 of call long 1 of put

    spread printValue 100

    spread volatility 0.3 printValue 100

    spread time 15 printValue 100

    println(call calculatePL 100)


    Plot of spread min 10 max 30 xAxis "time" yAxis "Value" show()

  }
}
