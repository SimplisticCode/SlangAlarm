// #Sireum

package playground.CashDispenserPP
import org.sireum._
import org.sireum.ops.ISZOps

@record class Letterbox() {
  var statements: ISZ[Letter] = ISZ();

  def PostStatement(letter: Letter): Unit = {
    statements = statements ++ ISZ(letter)
  }

  @pure def GetLastStatement(): Letter = {
    Contract(
      Requires(statements.nonEmpty)
    )
    return ISZOps(statements).last
  }
}
