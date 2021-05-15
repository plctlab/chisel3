// SPDX-License-Identifier: Apache-2.0

package chiselTests.util.experimental.minimizer

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import chisel3.util.experimental.pla
import chiselTests.EndToEndSMTBaseSpec

trait MinimizerSpec extends EndToEndSMTBaseSpec {
  def minimizer: Minimizer

  class DecodeTestModule(table: TruthTable) extends Module {
    val i = IO(Input(UInt(table.table.head._1.getWidth.W)))
    val (unminimizedI, unminimizedO) = pla(table.table.toSeq)
    unminimizedI := i
    val minimizedO: UInt = decoder(minimizer, i, table)
    chisel3.experimental.verification.assert(
      // if input is legal, minimized and unminimized output should be same.
      (minimizedO === unminimizedO) |
        // if input is not legal, minimized output should be captured by default.
        (minimizedO === table.default)
    )
  }

  def minimizerTest(testcase: TruthTable, caseName: String) = {
    test(
      () => new DecodeTestModule(table = testcase),
      s"${minimizer.getClass.getSimpleName}.$caseName",
      success
    )
  }

  val case0 = TruthTable(
      Map(
        BitPat("b000") -> BitPat("b0"),
        // BitPat("b001") -> BitPat("b?"),  // same as default, can be omitted
        // BitPat("b010") -> BitPat("b?"),  // same as default, can be omitted
        BitPat("b011") -> BitPat("b0"),
        BitPat("b100") -> BitPat("b1"),
        BitPat("b101") -> BitPat("b1"),
        BitPat("b110") -> BitPat("b0"),
        BitPat("b111") -> BitPat("b1")
      ),
      BitPat("b?")
    )

  val case1 = TruthTable(
      Map(
        BitPat("b000") -> BitPat("b0"),
        BitPat("b001") -> BitPat("b0"),
        // BitPat("b010") -> BitPat("b?"),  // same as default, can be omitted
        (BitPat("b011") -> BitPat("b0")),
        // BitPat("b100") -> BitPat("b?"),  // same as default, can be omitted
        // BitPat("b101") -> BitPat("b?"),  // same as default, can be omitted
        BitPat("b110") -> BitPat("b1"),
        BitPat("b111") -> BitPat("b0")
      ),
      BitPat("b?")
    )

  val multiDefaultCase = TruthTable(
    Map(
      BitPat("b000") -> BitPat("b0100"),
      BitPat("b001") -> BitPat("b?111"),
      BitPat("b010") -> BitPat("b?000"),
      BitPat("b011") -> BitPat("b0101"),
      BitPat("b111") -> BitPat("b1101")
    ),
    BitPat("b?100")
  )
}
