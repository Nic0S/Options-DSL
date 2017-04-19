

object Tester extends OptionsDSL {
  def main ( args: Array[String]) = {
    // Option.put("NVDA").strike(95.5).expiration(10)
    val op = Option call "NVDA" strike 100 expiration 30 contracts 1 volatility .25

    val sp = Spread of op and (op strike 150) and (op strike 165)

    println(sp)

  }
}