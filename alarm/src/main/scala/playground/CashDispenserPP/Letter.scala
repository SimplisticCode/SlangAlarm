// #Sireum
package CashDispenserPP

import org.sireum._

@record class Letter(val Address: String, val Name: String, val date: String, val transactions: ISZ[Transaction], val balance: Z) {}
