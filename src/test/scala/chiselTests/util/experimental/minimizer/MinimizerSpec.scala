// SPDX-License-Identifier: Apache-2.0

package chiselTests.util.experimental.minimizer

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import chisel3.util.experimental.pla
import chiselTests.EndToEndSMTBaseSpec

class DecodeTestModule(minimizer: Minimizer, table: TruthTable) extends Module {
  val i = IO(Input(UInt(table.table.head._1.getWidth.W)))
  val (unminimizedI, unminimizedO) = pla(table.table.toSeq)
  unminimizedI := i
  val minimizedO: UInt = decoder(minimizer, i, table)

  chisel3.experimental.verification.assert(
    // for each instruction, if input matches, output should match, not no matched, fallback to default
    (table.table.map { case (key, value) => (i === key) && (minimizedO === value) } ++
      Seq(table.table.keys.map(i =/= _).reduce(_ && _) && minimizedO === table.default)).reduce(_ || _)
  )
}

trait MinimizerSpec extends EndToEndSMTBaseSpec {
  def minimizer: Minimizer

  def minimizerTest(testcase: TruthTable, caseName: String) = {
    test(
      () => new DecodeTestModule(minimizer, table = testcase),
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

  val rv32iCase = {
    val BEQ       = BitPat("b?????????????????000?????1100011")
    val BNE       = BitPat("b?????????????????001?????1100011")
    val BLT       = BitPat("b?????????????????100?????1100011")
    val BGE       = BitPat("b?????????????????101?????1100011")
    val BLTU      = BitPat("b?????????????????110?????1100011")
    val BGEU      = BitPat("b?????????????????111?????1100011")
    val JALR      = BitPat("b?????????????????000?????1100111")
    val JAL       = BitPat("b?????????????????????????1101111")
    val LUI       = BitPat("b?????????????????????????0110111")
    val AUIPC     = BitPat("b?????????????????????????0010111")
    val ADDI      = BitPat("b?????????????????000?????0010011")
    val SLTI      = BitPat("b?????????????????010?????0010011")
    val SLTIU     = BitPat("b?????????????????011?????0010011")
    val XORI      = BitPat("b?????????????????100?????0010011")
    val ORI       = BitPat("b?????????????????110?????0010011")
    val ANDI      = BitPat("b?????????????????111?????0010011")
    val ADD       = BitPat("b0000000??????????000?????0110011")
    val SUB       = BitPat("b0100000??????????000?????0110011")
    val SLL       = BitPat("b0000000??????????001?????0110011")
    val SLT       = BitPat("b0000000??????????010?????0110011")
    val SLTU      = BitPat("b0000000??????????011?????0110011")
    val XOR       = BitPat("b0000000??????????100?????0110011")
    val SRL       = BitPat("b0000000??????????101?????0110011")
    val SRA       = BitPat("b0100000??????????101?????0110011")
    val OR        = BitPat("b0000000??????????110?????0110011")
    val AND       = BitPat("b0000000??????????111?????0110011")
    val LB        = BitPat("b?????????????????000?????0000011")
    val LH        = BitPat("b?????????????????001?????0000011")
    val LW        = BitPat("b?????????????????010?????0000011")
    val LBU       = BitPat("b?????????????????100?????0000011")
    val LHU       = BitPat("b?????????????????101?????0000011")
    val SB        = BitPat("b?????????????????000?????0100011")
    val SH        = BitPat("b?????????????????001?????0100011")
    val SW        = BitPat("b?????????????????010?????0100011")
    val FENCE     = BitPat("b?????????????????000?????0001111")
    val MRET      = BitPat("b00110000001000000000000001110011")
    val WFI       = BitPat("b00010000010100000000000001110011")
    val CEASE     = BitPat("b00110000010100000000000001110011")
    val CSRRW     = BitPat("b?????????????????001?????1110011")
    val CSRRS     = BitPat("b?????????????????010?????1110011")
    val CSRRC     = BitPat("b?????????????????011?????1110011")
    val CSRRWI    = BitPat("b?????????????????101?????1110011")
    val CSRRSI    = BitPat("b?????????????????110?????1110011")
    val CSRRCI    = BitPat("b?????????????????111?????1110011")
    val SCALL     = BitPat("b00000000000000000000000001110011")
    val SBREAK    = BitPat("b00000000000100000000000001110011")
    val SLLI_RV32 = BitPat("b0000000??????????001?????0010011")
    val SRLI_RV32 = BitPat("b0000000??????????101?????0010011")
    val SRAI_RV32 = BitPat("b0100000??????????101?????0010011")

    val A1_X    = "??"
    val A1_ZERO = "00"
    val A1_RS1  = "01"
    val A1_PC   = "10"

    val IMM_X  = "???"
    val IMM_S  = "000"
    val IMM_SB = "001"
    val IMM_U  = "010"
    val IMM_UJ = "011"
    val IMM_I  = "100"
    val IMM_Z  = "101"

    val A2_X    = "??"
    val A2_ZERO = "00"
    val A2_SIZE = "01"
    val A2_RS2  = "10"
    val A2_IMM  = "11"

    val X = "?"
    val N = "0"
    val Y = "1"

    val DW_X   = X
    val DW_XPR = Y

    val M_X   = "?????"
    val M_XRD = "00000"
    val M_XWR = "00001"

    val CSR_X = "???"
    val CSR_N = "000"
    val CSR_I = "100"
    val CSR_W = "101"
    val CSR_S = "110"
    val CSR_C = "111"

    val FN_X    = "????"
    val FN_ADD  = "0000"
    val FN_SL   = "0001"
    val FN_SEQ  = "0010"
    val FN_SNE  = "0011"
    val FN_XOR  = "0100"
    val FN_SR   = "0101"
    val FN_OR   = "0110"
    val FN_AND  = "0111"
    val FN_SUB  = "1010"
    val FN_SRA  = "1011"
    val FN_SLT  = "1100"
    val FN_SGE  = "1101"
    val FN_SLTU = "1110"
    val FN_SGEU = "1111"

    def bitPat(str: String*): BitPat = BitPat("b" + str.reduce(_ + _))
    TruthTable(
      Map(
        BNE->       bitPat(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR_N,N,N,N,N),
        BEQ->       bitPat(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR_N,N,N,N,N),
        BLT->       bitPat(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR_N,N,N,N,N),
        BLTU->      bitPat(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR_N,N,N,N,N),
        BGE->       bitPat(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR_N,N,N,N,N),
        BGEU->      bitPat(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR_N,N,N,N,N),

        JAL->       bitPat(Y,N,N,N,Y,N,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        JALR->      bitPat(Y,N,N,N,N,Y,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        AUIPC->     bitPat(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),

        LB->        bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        LH->        bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        LW->        bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        LBU->       bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        LHU->       bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SB->        bitPat(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR_N,N,N,N,N),
        SH->        bitPat(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR_N,N,N,N,N),
        SW->        bitPat(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR_N,N,N,N,N),

        LUI->       bitPat(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        ADDI->      bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SLTI ->     bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SLTIU->     bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        ANDI->      bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        ORI->       bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        XORI->      bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        ADD->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SUB->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SLT->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SLTU->      bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        AND->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        OR->        bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        XOR->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SLL->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SRL->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SRA->       bitPat(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),

        FENCE->     bitPat(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR_N,N,Y,N,N),

        SCALL->     bitPat(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR_I,N,N,N,N),
        SBREAK->    bitPat(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR_I,N,N,N,N),
        MRET->      bitPat(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR_I,N,N,N,N),
        WFI->       bitPat(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR_I,N,N,N,N),
        CEASE->     bitPat(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR_I,N,N,N,N),
        CSRRW->     bitPat(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_W,N,N,N,N),
        CSRRS->     bitPat(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_S,N,N,N,N),
        CSRRC->     bitPat(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_C,N,N,N,N),
        CSRRWI->    bitPat(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_W,N,N,N,N),
        CSRRSI->    bitPat(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_S,N,N,N,N),
        CSRRCI->    bitPat(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR_C,N,N,N,N),

        SLLI_RV32-> bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SRLI_RV32-> bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
        SRAI_RV32-> bitPat(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR_N,N,N,N,N),
      ),
      bitPat(N, X, X, X, X, X, X, X, X, A2_X, A1_X, IMM_X, DW_X, FN_X, N, M_X, X, X, X, X, X, X, X, CSR_X, X, X, X, X),
    )
  }
}
