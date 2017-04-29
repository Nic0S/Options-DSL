import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.JFrame

import info.monitorenter.gui.chart.IAxis.AxisTitle
import info.monitorenter.gui.chart.axis.scalepolicy.AxisScalePolicyAutomaticBestFit
import info.monitorenter.gui.chart.rangepolicies.RangePolicyFixedViewport
import info.monitorenter.gui.chart.traces.{Trace2DBijective, Trace2DLtd, Trace2DSimple}
import info.monitorenter.gui.chart.{Chart2D, ITrace2D}
import info.monitorenter.util.Range

class SpreadPlot {

  var spread : Spread = _
  var pl : Boolean = true
  var xAxis : String = "underlying"

  var min : Double = -1
  var max : Double = -1
  var step : Integer = -1

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
      copy.xAxis = "underlying"
    } else if (t.toLowerCase().equals("time")) {
      copy.xAxis = "time"
    } else if (t.toLowerCase().equals("volatility")) {
      copy.xAxis = "volatility"
    }
    copy
  }

  def min (m : Double) : SpreadPlot = {
    val copy : SpreadPlot = this.copy()
    copy.min = m
    copy
  }

  def max (m : Double) : SpreadPlot = {
    val copy : SpreadPlot = this.copy()
    copy.max = m
    copy
  }

  def copy () : SpreadPlot = {
    val c : SpreadPlot = new SpreadPlot
    c.spread = spread
    c.pl = pl
    c.xAxis = xAxis
    c.min = min
    c.max = max
    c
  }

  def show () : Unit = {
    val chart : Chart2D = new Chart2D
    val trace : ITrace2D = new Trace2DSimple
    chart addTrace trace
    chart.getAxisY.setRangePolicy(new RangePolicyFixedViewport(new Range(1, 100)))
    chart.getAxisY().setAxisTitle(new AxisTitle("PL"))

    var minYVal : Double = Double.MaxValue
    var maxYVal : Double = Double.MinValue

    var xTitle = ""
    var yTitle = ""
    if (xAxis.equals("underlying")) {
      xTitle = "Underlying Price"
      if (min == -1) {
        min = spread.options.head.strikePrice * 0.8
      }
      if (max == -1 ) {
        max = spread.options.head.strikePrice * 1.2
      }

      val step : Double = (max - min) / 100

      var price : Double = min
      while (price < max) {
        var yVal : Double = -1
        if (pl) {
          yTitle = "Profit / Loss"
          yVal = spread.calculatePL(price)
        } else {
          yTitle = "Value"
          yVal = spread.calculateValue(price)
        }

        if (yVal < minYVal) {
          minYVal = yVal
        }
        if (yVal > maxYVal) {
          maxYVal = yVal
        }

        trace.addPoint(price, yVal)

        price += step
      }
      if (minYVal < 0) {
        minYVal *= 1.3
      } else {
        minYVal *= 0.7
      }

      if (maxYVal < 0) {
        maxYVal *=   .7
      } else {
        minYVal *= 1.3
      }


    } else if (xAxis.equals("time")) {
      xTitle = "Time since purchase"
      require(spread.options.head.origUnderlyingPrice != -1,
        "Original underlying price must be set for time graph, use fillCost")

      if (min == -1) {
        min = 0
      }
      if (max == -1) {
        max = spread.options.head.daysToExp
      }
      if (step == -1) {
        step = 1
      }

      var time : Integer = min.toInt

      while (time < max) {
        var yVal : Double = -1
        if (pl) {
          yTitle = "Profit / Loss"
          yVal = spread.time(time).calculatePL(spread.options.head.origUnderlyingPrice)

        } else {
          yTitle = "Value"
          yVal = spread.time(time).calculateValue(spread.options.head.origUnderlyingPrice)
        }
        trace.addPoint(time.toDouble, yVal)

        if (yVal < minYVal) {
          minYVal = yVal
        }
        if (yVal > maxYVal) {
          maxYVal = yVal
        }

        time += step
      }
    }

    chart.getAxisX().setAxisTitle(new AxisTitle(xTitle))
    chart.getAxisY().setAxisTitle(new AxisTitle(yTitle))
    chart.getAxisY.setRangePolicy(new RangePolicyFixedViewport(new Range(minYVal, maxYVal)))

    val zeroTrace : ITrace2D = new Trace2DSimple
    chart addTrace zeroTrace
    zeroTrace.addPoint(min, 0)
    zeroTrace.addPoint(max, 0)

    val frame : JFrame = new JFrame("Chart");

    frame.getContentPane().add(chart)
    frame.setSize(400, 300)
    frame.addWindowListener(
      new WindowAdapter() {
        override def windowClosing (e : WindowEvent) : Unit = {
          System.exit(0)
        }
      }
    )
    frame.setVisible(true)
  }
}
