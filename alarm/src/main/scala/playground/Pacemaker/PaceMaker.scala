// #Sireum

package playground.Pacemaker

import org.sireum._

@record class PaceMaker() {
  val wrongTR: Trace = Trace(ISZ(Some(Event.A), None(), Some(Event.V),
    None(), None(), Some(Event.A), None(), None(), None(), None()))

  def Pace(tr: Trace, aperi: Z, vdel: Z): Trace = {
    var trace: ISZ[Option[Event.Type]] = ISZ(None())
    for (i <- tr.Trace.indices) {
      if ((i % aperi == (vdel + 1)) && (tr(i) != Event.V)) {
        trace = trace ++ ISZ(Some(Event.V))
      } else {
        trace = trace ++ ISZ(None())
      }
    }
    return Trace(trace)
  }

}