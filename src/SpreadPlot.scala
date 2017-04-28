
class SpreadPlot {

  var spread : Spread = _
  var pl : Boolean = true
  var underlying : Boolean = true

  def yAxis (t : String) : SpreadPlot = {
    val copy : SpreadPlot = this.copy()
    if (t.toLowerCase().equals("pl")) {
      copy.pl = true
    } else if (t.toLowerCase().equals("value")){
      copy.pl = false
    }
    copy
  }

  def xAxis (t : String) : SpreadPlot = {
    val copy : SpreadPlot = this.copy()
    if (t.toLowerCase().equals("underlying")) {
      copy.underlying = true
    } else if (t.toLowerCase().equals("time")) {
      copy.underlying = false
    }
    copy
  }

  def copy () : SpreadPlot = {
    val c : SpreadPlot = new SpreadPlot
    c.spread = spread
    c.pl = pl
    c
  }
}
