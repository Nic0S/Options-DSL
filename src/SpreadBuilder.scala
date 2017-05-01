
class SpreadBuilder {
  var count : Integer = 0

  var spread : Spread = null

  def of (op : Option): Spread = {
    require(count != 0, "SpreadBuilder without count provided")

    if (spread == null) {
      if (op.costBasis > 0) {
        op.costBasis = -op.costBasis
      }
      new Spread and (op contracts count)
    } else {
      if (op.costBasis > 0) {
        op.costBasis = -op.costBasis
      }
      spread and (op contracts count)
    }
  }
}
