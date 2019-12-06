// #Sireum

package playground.Pacemaker

import org.sireum._

@enum object Event {
  'A
  'V
}

@record class Trace(var Trace: ISZ[Option[Event.Type]])

@record class Heart(var aperiod: Z, var vdelay: Z) {

  def IdealHeart(): Trace = {
    var trace: ISZ[Option[Event.Type]] = ISZ()
    for (i <- 1 until 100) {
      if (i % aperiod == 1) {
        trace = trace ++ ISZ(Some(Event.A))
      } else if (i % aperiod == (vdelay + 1)) {
        trace = trace ++ ISZ(Some(Event.V))
      } else {
        trace = trace ++ ISZ(None())
      }
    }
    return Trace(trace)
  }

  def Periodic(tr: ISZ[Event.Type], e: Event.Type, p: Z): B = {
    All(tr.indices.filter(t => (tr(t) == e) imply_:((t + p)<=tr.size)))(t => ((tr(t+p) == e) & All(for(i <- t until (t + p - 1)))() )
    for (t <- tr.indices) {
      if (tr(t) == e) {
        if (t + p <= tr.size) {
          if (tr(t + p) == e) {
            for (i <- t until (t + p - 1)) {
              if (tr(i) == e) {
                return F
              }
            }
          } else {
            return F
          }
        }
      }
    }
    return T
  }
}