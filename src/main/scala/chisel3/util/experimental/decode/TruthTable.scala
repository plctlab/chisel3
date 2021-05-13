// SPDX-License-Identifier: Apache-2.0

package chisel3.util.experimental.decode

import chisel3.util.BitPat

case class TruthTable(table: Map[BitPat, BitPat], default: BitPat) {
  require(table.map(_._1.getWidth).toSet.size == 1, "input width not equal.")
  require(table.map(_._2.getWidth).toSet.size == 1, "output width not equal.")

  def inputWidth = table.head._1.getWidth

  def outputWidth = table.head._2.getWidth

  override def toString: String = {
    def writeRow(map: (BitPat, BitPat)): String =
      s"${simpleBitPat(map._1)}->${simpleBitPat(map._2)}"

    def simpleBitPat(bitPat: BitPat): String =
      bitPat.toString.drop(7).dropRight(1)

    (table.map(writeRow) ++ Seq(s"default->${simpleBitPat(default)}")).mkString("\n")
  }
}

object TruthTable {
  private def bpStr(bitPat: BitPat) = bitPat.toString.drop(7).dropRight(1)

  /** consume 1 table, split it into three tables with index correspond to original table:
    * 0 -> on
    * 1 -> off
    * 2 -> dc
    *
    * @note
    * Since most of minimizer(like espresso) cannot handle a multiple default table.
    * It is useful to split a table into 3 tables based on the default type.
    */
  private[decode] def split(
    table: TruthTable
  ): ((TruthTable, Seq[Int]), (TruthTable, Seq[Int]), (TruthTable, Seq[Int])) = {
    def bpFilter(bitPat: BitPat, indexes: Seq[Int]): BitPat =
      BitPat(s"b${bpStr(bitPat).zipWithIndex.filter(b => indexes.contains(b._2)).map(_._1).mkString}")

    def tableFilter(truthTable: TruthTable, indexes: Seq[Int]): TruthTable =
      TruthTable(
        truthTable.table.map { case (in, out) => in -> bpFilter(out, indexes) },
        bpFilter(truthTable.default, indexes)
      )

    def index(bitPat: BitPat, bpType: Char): Seq[Int] =
      bpStr(bitPat).zipWithIndex.filter(_._1 == bpType).map(_._2)

    val onIndexes = index(table.default, '1')
    val offIndexes = index(table.default, '0')
    val dontCareIndexes = index(table.default, '?')

    (
      (tableFilter(table, onIndexes), onIndexes),
      (tableFilter(table, offIndexes), offIndexes),
      (tableFilter(table, dontCareIndexes), dontCareIndexes)
    )
  }

  /** consume 3 tables, merge it into single table with different default bits.
    *
    * @note
    * Since most of minimizer(like espresso) cannot handle a multiple default table.
    * It is useful to split a table into 3 tables based on the default type.
    */
  private[decode] def merge(
    on:       (TruthTable, Seq[Int]),
    off:      (TruthTable, Seq[Int]),
    dontCare: (TruthTable, Seq[Int])
  ): TruthTable = {
    def reIndex(bitPat: BitPat, table: TruthTable, indexes: Seq[Int]): Seq[(Char, Int)] =
      bpStr(table.table.getOrElse(bitPat, BitPat.dontCare(indexes.size))).zip(indexes)
    def bitPat(indexedChar: Seq[(Char, Int)]) = BitPat(s"b${indexedChar
      .sortBy(_._2)
      .map(_._1)
      .mkString}")
    val all = Seq(on, off, dontCare)
    TruthTable(
      all
        .flatMap(_._1.table.keys)
        .map { key =>
          key -> bitPat(all.flatMap { case (table, indexes) => reIndex(key, table, indexes) })
        }
        .toMap,
      bitPat(all.flatMap { case (table, indexes) => bpStr(table.default).zip(indexes) })
    )
  }
}
