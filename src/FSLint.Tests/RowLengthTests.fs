namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// A binding body runs to at most forty-two rows. Past that it is asked to be
/// broken up, and the report names the binding rather than underlining forty
/// rows of it.
///
/// What is written out rather than done is passed over: a table, a record, a
/// `seq`, and whatever shapes one on the way. So is a test, which is one
/// scenario and means nothing cut in three.
///
/// So is a body built around an enumeration -- a `match` or a `while` running
/// past twenty rows, which takes its length from how many cases or steps there
/// are and is no shorter for the body around it being split. A shorter one is
/// a branch taken in passing and rescues nothing, or the rows in front of it
/// would never be measured at all.
///
/// The search for one covers whatever the body's rows were counted from: names
/// bound, statements sequenced, pipes threaded, the bodies of loops and of
/// `try`, and either branch of a conditional. It stops at an arrow, a lambda,
/// and a call's argument.
[<TestClass>]
type RowLengthTests() =

  /// Forty-two rows is the budget, and a body is measured to the row. This
  /// one spends every row it has and is not asked for anything.
  let goodBudgetTest =
    """
    let liftMultiplyLong (ins: Instruction) insLen bld =
      bld <!-- (ins.Address, insLen)
      bld <+ (t01 := src1 .+ numI32 1 32<rt>)
      bld <+ (t02 := src2 .+ numI32 2 32<rt>)
      bld <+ (t03 := src3 .+ numI32 3 32<rt>)
      bld <+ (t04 := src0 .+ numI32 4 32<rt>)
      bld <+ (t05 := src1 .+ numI32 5 32<rt>)
      bld <+ (t06 := src2 .+ numI32 6 32<rt>)
      bld <+ (t07 := src3 .+ numI32 7 32<rt>)
      bld <+ (t08 := src0 .+ numI32 8 32<rt>)
      bld <+ (t09 := src1 .+ numI32 9 32<rt>)
      bld <+ (t10 := src2 .+ numI32 10 32<rt>)
      bld <+ (t11 := src3 .+ numI32 11 32<rt>)
      bld <+ (t12 := src0 .+ numI32 12 32<rt>)
      bld <+ (t13 := src1 .+ numI32 13 32<rt>)
      bld <+ (t14 := src2 .+ numI32 14 32<rt>)
      bld <+ (t15 := src3 .+ numI32 15 32<rt>)
      bld <+ (t16 := src0 .+ numI32 16 32<rt>)
      bld <+ (t17 := src1 .+ numI32 17 32<rt>)
      bld <+ (t18 := src2 .+ numI32 18 32<rt>)
      bld <+ (t19 := src3 .+ numI32 19 32<rt>)
      bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      bld <+ (t21 := src1 .+ numI32 21 32<rt>)
      bld <+ (t22 := src2 .+ numI32 22 32<rt>)
      bld <+ (t23 := src3 .+ numI32 23 32<rt>)
      bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      bld <+ (t25 := src1 .+ numI32 25 32<rt>)
      bld <+ (t26 := src2 .+ numI32 26 32<rt>)
      bld <+ (t27 := src3 .+ numI32 27 32<rt>)
      bld <+ (t28 := src0 .+ numI32 28 32<rt>)
      bld <+ (t29 := src1 .+ numI32 29 32<rt>)
      bld <+ (t30 := src2 .+ numI32 30 32<rt>)
      bld <+ (t31 := src3 .+ numI32 31 32<rt>)
      bld <+ (t32 := src0 .+ numI32 32 32<rt>)
      bld <+ (t33 := src1 .+ numI32 33 32<rt>)
      bld <+ (t34 := src2 .+ numI32 34 32<rt>)
      bld <+ (t35 := src3 .+ numI32 35 32<rt>)
      bld <+ (t36 := src0 .+ numI32 36 32<rt>)
      bld <+ (t37 := src1 .+ numI32 37 32<rt>)
      bld <+ (t38 := src2 .+ numI32 38 32<rt>)
      bld <+ (t39 := src3 .+ numI32 39 32<rt>)
      bld <+ (t40 := src0 .+ numI32 40 32<rt>)
      bld --!> insLen
"""

  /// One row more than the budget, and the same body is asked to be broken
  /// up. Nothing about it has changed but its length.
  let badOverBudgetTest =
    """
    let liftMultiplyLong (ins: Instruction) insLen bld =
      bld <!-- (ins.Address, insLen)
      bld <+ (t01 := src1 .+ numI32 1 32<rt>)
      bld <+ (t02 := src2 .+ numI32 2 32<rt>)
      bld <+ (t03 := src3 .+ numI32 3 32<rt>)
      bld <+ (t04 := src0 .+ numI32 4 32<rt>)
      bld <+ (t05 := src1 .+ numI32 5 32<rt>)
      bld <+ (t06 := src2 .+ numI32 6 32<rt>)
      bld <+ (t07 := src3 .+ numI32 7 32<rt>)
      bld <+ (t08 := src0 .+ numI32 8 32<rt>)
      bld <+ (t09 := src1 .+ numI32 9 32<rt>)
      bld <+ (t10 := src2 .+ numI32 10 32<rt>)
      bld <+ (t11 := src3 .+ numI32 11 32<rt>)
      bld <+ (t12 := src0 .+ numI32 12 32<rt>)
      bld <+ (t13 := src1 .+ numI32 13 32<rt>)
      bld <+ (t14 := src2 .+ numI32 14 32<rt>)
      bld <+ (t15 := src3 .+ numI32 15 32<rt>)
      bld <+ (t16 := src0 .+ numI32 16 32<rt>)
      bld <+ (t17 := src1 .+ numI32 17 32<rt>)
      bld <+ (t18 := src2 .+ numI32 18 32<rt>)
      bld <+ (t19 := src3 .+ numI32 19 32<rt>)
      bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      bld <+ (t21 := src1 .+ numI32 21 32<rt>)
      bld <+ (t22 := src2 .+ numI32 22 32<rt>)
      bld <+ (t23 := src3 .+ numI32 23 32<rt>)
      bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      bld <+ (t25 := src1 .+ numI32 25 32<rt>)
      bld <+ (t26 := src2 .+ numI32 26 32<rt>)
      bld <+ (t27 := src3 .+ numI32 27 32<rt>)
      bld <+ (t28 := src0 .+ numI32 28 32<rt>)
      bld <+ (t29 := src1 .+ numI32 29 32<rt>)
      bld <+ (t30 := src2 .+ numI32 30 32<rt>)
      bld <+ (t31 := src3 .+ numI32 31 32<rt>)
      bld <+ (t32 := src0 .+ numI32 32 32<rt>)
      bld <+ (t33 := src1 .+ numI32 33 32<rt>)
      bld <+ (t34 := src2 .+ numI32 34 32<rt>)
      bld <+ (t35 := src3 .+ numI32 35 32<rt>)
      bld <+ (t36 := src0 .+ numI32 36 32<rt>)
      bld <+ (t37 := src1 .+ numI32 37 32<rt>)
      bld <+ (t38 := src2 .+ numI32 38 32<rt>)
      bld <+ (t39 := src3 .+ numI32 39 32<rt>)
      bld <+ (t40 := src0 .+ numI32 40 32<rt>)
      bld <+ (t41 := src1 .+ numI32 41 32<rt>)
      bld --!> insLen
"""

  /// A table is no easier to read for being cut in three, and there is no
  /// smaller function to take out of one. What is long about a table is its
  /// width on the page, and the line budget already answers that.
  let goodArrayTableTest =
    """
    let init () =
      let rid = Intel.Register.toRegID
      [| { RID = rid R00; RType = 32<rt>; Size = 4; Offset = 0 }
         { RID = rid R01; RType = 32<rt>; Size = 4; Offset = 4 }
         { RID = rid R02; RType = 32<rt>; Size = 4; Offset = 8 }
         { RID = rid R03; RType = 32<rt>; Size = 4; Offset = 12 }
         { RID = rid R04; RType = 32<rt>; Size = 4; Offset = 16 }
         { RID = rid R05; RType = 32<rt>; Size = 4; Offset = 20 }
         { RID = rid R06; RType = 32<rt>; Size = 4; Offset = 24 }
         { RID = rid R07; RType = 32<rt>; Size = 4; Offset = 28 }
         { RID = rid R08; RType = 32<rt>; Size = 4; Offset = 32 }
         { RID = rid R09; RType = 32<rt>; Size = 4; Offset = 36 }
         { RID = rid R10; RType = 32<rt>; Size = 4; Offset = 40 }
         { RID = rid R11; RType = 32<rt>; Size = 4; Offset = 44 }
         { RID = rid R12; RType = 32<rt>; Size = 4; Offset = 48 }
         { RID = rid R13; RType = 32<rt>; Size = 4; Offset = 52 }
         { RID = rid R14; RType = 32<rt>; Size = 4; Offset = 56 }
         { RID = rid R15; RType = 32<rt>; Size = 4; Offset = 60 }
         { RID = rid R16; RType = 32<rt>; Size = 4; Offset = 64 }
         { RID = rid R17; RType = 32<rt>; Size = 4; Offset = 68 }
         { RID = rid R18; RType = 32<rt>; Size = 4; Offset = 72 }
         { RID = rid R19; RType = 32<rt>; Size = 4; Offset = 76 }
         { RID = rid R20; RType = 32<rt>; Size = 4; Offset = 80 }
         { RID = rid R21; RType = 32<rt>; Size = 4; Offset = 84 }
         { RID = rid R22; RType = 32<rt>; Size = 4; Offset = 88 }
         { RID = rid R23; RType = 32<rt>; Size = 4; Offset = 92 }
         { RID = rid R24; RType = 32<rt>; Size = 4; Offset = 96 }
         { RID = rid R25; RType = 32<rt>; Size = 4; Offset = 100 }
         { RID = rid R26; RType = 32<rt>; Size = 4; Offset = 104 }
         { RID = rid R27; RType = 32<rt>; Size = 4; Offset = 108 }
         { RID = rid R28; RType = 32<rt>; Size = 4; Offset = 112 }
         { RID = rid R29; RType = 32<rt>; Size = 4; Offset = 116 }
         { RID = rid R30; RType = 32<rt>; Size = 4; Offset = 120 }
         { RID = rid R31; RType = 32<rt>; Size = 4; Offset = 124 }
         { RID = rid R32; RType = 32<rt>; Size = 4; Offset = 128 }
         { RID = rid R33; RType = 32<rt>; Size = 4; Offset = 132 }
         { RID = rid R34; RType = 32<rt>; Size = 4; Offset = 136 }
         { RID = rid R35; RType = 32<rt>; Size = 4; Offset = 140 }
         { RID = rid R36; RType = 32<rt>; Size = 4; Offset = 144 }
         { RID = rid R37; RType = 32<rt>; Size = 4; Offset = 148 }
         { RID = rid R38; RType = 32<rt>; Size = 4; Offset = 152 }
         { RID = rid R39; RType = 32<rt>; Size = 4; Offset = 156 }
         { RID = rid R40; RType = 32<rt>; Size = 4; Offset = 160 }
         { RID = rid R41; RType = 32<rt>; Size = 4; Offset = 164 }
         { RID = rid R42; RType = 32<rt>; Size = 4; Offset = 168 }
         { RID = rid R43; RType = 32<rt>; Size = 4; Offset = 172 }
         { RID = rid R44; RType = 32<rt>; Size = 4; Offset = 176 }
         { RID = rid R45; RType = 32<rt>; Size = 4; Offset = 180 }
         { RID = rid R46; RType = 32<rt>; Size = 4; Offset = 184 }
         { RID = rid R47; RType = 32<rt>; Size = 4; Offset = 188 }
         { RID = rid R48; RType = 32<rt>; Size = 4; Offset = 192 }
         { RID = rid R49; RType = 32<rt>; Size = 4; Offset = 196 }
         { RID = rid R50; RType = 32<rt>; Size = 4; Offset = 200 } |]
"""

  /// Which side of an `=` a table was written on makes no difference, and
  /// which brackets were typed says nothing about what stands in them.
  let goodRecordTableTest =
    """
    let emptyHeader =
      { Magic = 0u
        Field01 = 1u
        Field02 = 2u
        Field03 = 3u
        Field04 = 4u
        Field05 = 5u
        Field06 = 6u
        Field07 = 7u
        Field08 = 8u
        Field09 = 9u
        Field10 = 10u
        Field11 = 11u
        Field12 = 12u
        Field13 = 13u
        Field14 = 14u
        Field15 = 15u
        Field16 = 16u
        Field17 = 17u
        Field18 = 18u
        Field19 = 19u
        Field20 = 20u
        Field21 = 21u
        Field22 = 22u
        Field23 = 23u
        Field24 = 24u
        Field25 = 25u
        Field26 = 26u
        Field27 = 27u
        Field28 = 28u
        Field29 = 29u
        Field30 = 30u
        Field31 = 31u
        Field32 = 32u
        Field33 = 33u
        Field34 = 34u
        Field35 = 35u
        Field36 = 36u
        Field37 = 37u
        Field38 = 38u
        Field39 = 39u
        Field40 = 40u
        Field41 = 41u
        Field42 = 42u
        Field43 = 43u
        Field44 = 44u
        Field45 = 45u
        Field46 = 46u
        Field47 = 47u
        Field48 = 48u
        Field49 = 49u
        Checksum = 0u }
"""

  /// A table handed on by a pipe is the table still. `[| ... |] |> ofElems`
  /// says what `ofElems [| ... |]` says, and which way round it was written
  /// tells nothing about what is being read.
  let goodPipedTableTest =
    """
    let init () =
      let rid = ARM64.Register.toRegID
      [| { RID = rid X0; RType = 64<rt>; Size = 8; Offset = 0 }
         { RID = rid X1; RType = 64<rt>; Size = 8; Offset = 8 }
         { RID = rid X2; RType = 64<rt>; Size = 8; Offset = 16 }
         { RID = rid X3; RType = 64<rt>; Size = 8; Offset = 24 }
         { RID = rid X4; RType = 64<rt>; Size = 8; Offset = 32 }
         { RID = rid X5; RType = 64<rt>; Size = 8; Offset = 40 }
         { RID = rid X6; RType = 64<rt>; Size = 8; Offset = 48 }
         { RID = rid X7; RType = 64<rt>; Size = 8; Offset = 56 }
         { RID = rid X8; RType = 64<rt>; Size = 8; Offset = 64 }
         { RID = rid X9; RType = 64<rt>; Size = 8; Offset = 72 }
         { RID = rid X10; RType = 64<rt>; Size = 8; Offset = 80 }
         { RID = rid X11; RType = 64<rt>; Size = 8; Offset = 88 }
         { RID = rid X12; RType = 64<rt>; Size = 8; Offset = 96 }
         { RID = rid X13; RType = 64<rt>; Size = 8; Offset = 104 }
         { RID = rid X14; RType = 64<rt>; Size = 8; Offset = 112 }
         { RID = rid X15; RType = 64<rt>; Size = 8; Offset = 120 }
         { RID = rid X16; RType = 64<rt>; Size = 8; Offset = 128 }
         { RID = rid X17; RType = 64<rt>; Size = 8; Offset = 136 }
         { RID = rid X18; RType = 64<rt>; Size = 8; Offset = 144 }
         { RID = rid X19; RType = 64<rt>; Size = 8; Offset = 152 }
         { RID = rid X20; RType = 64<rt>; Size = 8; Offset = 160 }
         { RID = rid X21; RType = 64<rt>; Size = 8; Offset = 168 }
         { RID = rid X22; RType = 64<rt>; Size = 8; Offset = 176 }
         { RID = rid X23; RType = 64<rt>; Size = 8; Offset = 184 }
         { RID = rid X24; RType = 64<rt>; Size = 8; Offset = 192 }
         { RID = rid X25; RType = 64<rt>; Size = 8; Offset = 200 }
         { RID = rid X26; RType = 64<rt>; Size = 8; Offset = 208 }
         { RID = rid X27; RType = 64<rt>; Size = 8; Offset = 216 }
         { RID = rid X28; RType = 64<rt>; Size = 8; Offset = 224 }
         { RID = rid X29; RType = 64<rt>; Size = 8; Offset = 232 }
         { RID = rid X30; RType = 64<rt>; Size = 8; Offset = 240 }
         { RID = rid XZR; RType = 64<rt>; Size = 8; Offset = 248 }
         { RID = rid V0A; RType = 64<rt>; Size = 8; Offset = 256 }
         { RID = rid V1A; RType = 64<rt>; Size = 8; Offset = 264 }
         { RID = rid V2A; RType = 64<rt>; Size = 8; Offset = 272 }
         { RID = rid V3A; RType = 64<rt>; Size = 8; Offset = 280 }
         { RID = rid V4A; RType = 64<rt>; Size = 8; Offset = 288 }
         { RID = rid V5A; RType = 64<rt>; Size = 8; Offset = 296 }
         { RID = rid V6A; RType = 64<rt>; Size = 8; Offset = 304 }
         { RID = rid V7A; RType = 64<rt>; Size = 8; Offset = 312 }
         { RID = rid V8A; RType = 64<rt>; Size = 8; Offset = 320 }
         { RID = rid V9A; RType = 64<rt>; Size = 8; Offset = 328 }
         { RID = rid V10A; RType = 64<rt>; Size = 8; Offset = 336 }
         { RID = rid V11A; RType = 64<rt>; Size = 8; Offset = 344 }
         { RID = rid V12A; RType = 64<rt>; Size = 8; Offset = 352 }
         { RID = rid V13A; RType = 64<rt>; Size = 8; Offset = 360 }
         { RID = rid V14A; RType = 64<rt>; Size = 8; Offset = 368 }
         { RID = rid V15A; RType = 64<rt>; Size = 8; Offset = 376 }
         { RID = rid SP; RType = 64<rt>; Size = 8; Offset = 780 } |]
      |> LLVMContext.ofRegisterElements
"""

  /// A string spelled out down the page is a table of its own: help text, a
  /// banner, a sample of source. What is long about one is what it says,
  /// and there is no smaller function inside a quotation mark.
  let goodStringTableTest =
    """
    let usage =
      "Usage: b2r2 [app name]
       -- app number 01
       -- app number 02
       -- app number 03
       -- app number 04
       -- app number 05
       -- app number 06
       -- app number 07
       -- app number 08
       -- app number 09
       -- app number 10
       -- app number 11
       -- app number 12
       -- app number 13
       -- app number 14
       -- app number 15
       -- app number 16
       -- app number 17
       -- app number 18
       -- app number 19
       -- app number 20
       -- app number 21
       -- app number 22
       -- app number 23
       -- app number 24
       -- app number 25
       -- app number 26
       -- app number 27
       -- app number 28
       -- app number 29
       -- app number 30
       -- app number 31
       -- app number 32
       -- app number 33
       -- app number 34
       -- app number 35
       -- app number 36
       -- app number 37
       -- app number 38
       -- app number 39
       -- app number 40
       -- app number 41
       -- app number 42
       -- app number 43
       -- app number 44
       -- app number 45
       -- app number 46
       -- app number 47
       -- app number 48
       -- app number 49
       -- and that is all there is to say"
"""

  /// A literal handed straight to what shapes it is still the literal being
  /// read.
  let goodShapedTableTest =
    """
    let knownOpcodes =
      set [ Op.OP00
            Op.OP01
            Op.OP02
            Op.OP03
            Op.OP04
            Op.OP05
            Op.OP06
            Op.OP07
            Op.OP08
            Op.OP09
            Op.OP10
            Op.OP11
            Op.OP12
            Op.OP13
            Op.OP14
            Op.OP15
            Op.OP16
            Op.OP17
            Op.OP18
            Op.OP19
            Op.OP20
            Op.OP21
            Op.OP22
            Op.OP23
            Op.OP24
            Op.OP25
            Op.OP26
            Op.OP27
            Op.OP28
            Op.OP29
            Op.OP30
            Op.OP31
            Op.OP32
            Op.OP33
            Op.OP34
            Op.OP35
            Op.OP36
            Op.OP37
            Op.OP38
            Op.OP39
            Op.OP40
            Op.OP41
            Op.OP42
            Op.OP43
            Op.OP44
            Op.OP45
            Op.OP46
            Op.OP47
            Op.OP48
            Op.OP49
            Op.OP50 ]
"""

  /// That step is taken only where the call takes the one argument beside
  /// it. `f a b` nests to the left, so following the argument of a call
  /// whose head is itself a call would walk to the last argument and leave
  /// everything before it unread.
  let badTwoArgumentTableTest =
    """
    let register () =
      registerAll
        (let table = Dictionary()
         table[1] <- lift01
         table[2] <- lift02
         table[3] <- lift03
         table[4] <- lift04
         table[5] <- lift05
         table[6] <- lift06
         table[7] <- lift07
         table[8] <- lift08
         table[9] <- lift09
         table[10] <- lift10
         table[11] <- lift11
         table[12] <- lift12
         table[13] <- lift13
         table[14] <- lift14
         table[15] <- lift15
         table[16] <- lift16
         table[17] <- lift17
         table[18] <- lift18
         table[19] <- lift19
         table[20] <- lift20
         table[21] <- lift21
         table[22] <- lift22
         table[23] <- lift23
         table[24] <- lift24
         table[25] <- lift25
         table[26] <- lift26
         table[27] <- lift27
         table[28] <- lift28
         table[29] <- lift29
         table[30] <- lift30
         table[31] <- lift31
         table[32] <- lift32
         table[33] <- lift33
         table[34] <- lift34
         table[35] <- lift35
         table[36] <- lift36
         table[37] <- lift37
         table[38] <- lift38
         table[39] <- lift39
         table[40] <- lift40
         table[41] <- lift41
         table[42] <- lift42
         table[43] <- lift43
         table[44] <- lift44
         table)
        [ Op.OP00; Op.OP01 ]
"""

  /// `seq { ... }` names a table as much as `[ ... ]` does.
  let goodSeqTableTest =
    """
    let allRegisters () =
      seq {
        yield Register.R01
        yield Register.R02
        yield Register.R03
        yield Register.R04
        yield Register.R05
        yield Register.R06
        yield Register.R07
        yield Register.R08
        yield Register.R09
        yield Register.R10
        yield Register.R11
        yield Register.R12
        yield Register.R13
        yield Register.R14
        yield Register.R15
        yield Register.R16
        yield Register.R17
        yield Register.R18
        yield Register.R19
        yield Register.R20
        yield Register.R21
        yield Register.R22
        yield Register.R23
        yield Register.R24
        yield Register.R25
        yield Register.R26
        yield Register.R27
        yield Register.R28
        yield Register.R29
        yield Register.R30
        yield Register.R31
        yield Register.R32
        yield Register.R33
        yield Register.R34
        yield Register.R35
        yield Register.R36
        yield Register.R37
        yield Register.R38
        yield Register.R39
        yield Register.R40
        yield Register.R41
        yield Register.R42
        yield Register.R43
        yield Register.R44
        yield Register.R45
        yield Register.R46
        yield Register.R47
        yield Register.R48
        yield Register.R49
      }
"""

  /// `async` and `task` sequence real work, which is what the rule is here
  /// to measure.
  let badAsyncWorkTest =
    """
    let fetchAll url =
      async {
        let! part01 = fetch url 1
        let! part02 = fetch url 2
        let! part03 = fetch url 3
        let! part04 = fetch url 4
        let! part05 = fetch url 5
        let! part06 = fetch url 6
        let! part07 = fetch url 7
        let! part08 = fetch url 8
        let! part09 = fetch url 9
        let! part10 = fetch url 10
        let! part11 = fetch url 11
        let! part12 = fetch url 12
        let! part13 = fetch url 13
        let! part14 = fetch url 14
        let! part15 = fetch url 15
        let! part16 = fetch url 16
        let! part17 = fetch url 17
        let! part18 = fetch url 18
        let! part19 = fetch url 19
        let! part20 = fetch url 20
        let! part21 = fetch url 21
        let! part22 = fetch url 22
        let! part23 = fetch url 23
        let! part24 = fetch url 24
        let! part25 = fetch url 25
        let! part26 = fetch url 26
        let! part27 = fetch url 27
        let! part28 = fetch url 28
        let! part29 = fetch url 29
        let! part30 = fetch url 30
        let! part31 = fetch url 31
        let! part32 = fetch url 32
        let! part33 = fetch url 33
        let! part34 = fetch url 34
        let! part35 = fetch url 35
        let! part36 = fetch url 36
        let! part37 = fetch url 37
        let! part38 = fetch url 38
        let! part39 = fetch url 39
        let! part40 = fetch url 40
        let! part41 = fetch url 41
        let! part42 = fetch url 42
        let! part43 = fetch url 43
        let! part44 = fetch url 44
        return part01
      }
"""

  /// A long `match` is an enumeration of patterns, no shorter for the body
  /// around it being cut in two: splitting one renames the enumeration
  /// rather than shortening it. Its arms are never counted, however many
  /// of them there are.
  let goodLongMatchTest =
    """
    let lift (ins: Instruction) insLen bld =
      match ins.Opcode with
      | Op.OP01 -> lift01 ins insLen bld
      | Op.OP02 -> lift02 ins insLen bld
      | Op.OP03 -> lift03 ins insLen bld
      | Op.OP04 -> lift04 ins insLen bld
      | Op.OP05 -> lift05 ins insLen bld
      | Op.OP06 -> lift06 ins insLen bld
      | Op.OP07 -> lift07 ins insLen bld
      | Op.OP08 -> lift08 ins insLen bld
      | Op.OP09 -> lift09 ins insLen bld
      | Op.OP10 -> lift10 ins insLen bld
      | Op.OP11 -> lift11 ins insLen bld
      | Op.OP12 -> lift12 ins insLen bld
      | Op.OP13 -> lift13 ins insLen bld
      | Op.OP14 -> lift14 ins insLen bld
      | Op.OP15 -> lift15 ins insLen bld
      | Op.OP16 -> lift16 ins insLen bld
      | Op.OP17 -> lift17 ins insLen bld
      | Op.OP18 -> lift18 ins insLen bld
      | Op.OP19 -> lift19 ins insLen bld
      | Op.OP20 -> lift20 ins insLen bld
      | Op.OP21 -> lift21 ins insLen bld
      | Op.OP22 -> lift22 ins insLen bld
      | Op.OP23 -> lift23 ins insLen bld
      | Op.OP24 -> lift24 ins insLen bld
      | Op.OP25 -> lift25 ins insLen bld
      | Op.OP26 -> lift26 ins insLen bld
      | Op.OP27 -> lift27 ins insLen bld
      | Op.OP28 -> lift28 ins insLen bld
      | Op.OP29 -> lift29 ins insLen bld
      | Op.OP30 -> lift30 ins insLen bld
      | Op.OP31 -> lift31 ins insLen bld
      | Op.OP32 -> lift32 ins insLen bld
      | Op.OP33 -> lift33 ins insLen bld
      | Op.OP34 -> lift34 ins insLen bld
      | Op.OP35 -> lift35 ins insLen bld
      | Op.OP36 -> lift36 ins insLen bld
      | Op.OP37 -> lift37 ins insLen bld
      | Op.OP38 -> lift38 ins insLen bld
      | Op.OP39 -> lift39 ins insLen bld
      | Op.OP40 -> lift40 ins insLen bld
      | Op.OP41 -> lift41 ins insLen bld
      | Op.OP42 -> lift42 ins insLen bld
      | Op.OP43 -> lift43 ins insLen bld
      | Op.OP44 -> lift44 ins insLen bld
      | Op.OP45 -> lift45 ins insLen bld
      | Op.OP46 -> lift46 ins insLen bld
      | Op.OP47 -> lift47 ins insLen bld
      | Op.OP48 -> lift48 ins insLen bld
      | Op.OP49 -> lift49 ins insLen bld
      | Op.OP50 -> lift50 ins insLen bld
      | _ -> raise InvalidOpcodeException
"""

  /// A clause body answers for nothing, whatever it holds. Standing under
  /// an arrow is enough: what an enumeration lists is not read the way a
  /// function body is.
  let goodFatClauseTest =
    """
    let lift (ins: Instruction) insLen bld =
      match ins.Opcode with
      | Op.ADD ->
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
        bld <+ (t25 := src1 .+ numI32 25 32<rt>)
        bld <+ (t26 := src2 .+ numI32 26 32<rt>)
        bld <+ (t27 := src3 .+ numI32 27 32<rt>)
        bld <+ (t28 := src0 .+ numI32 28 32<rt>)
        bld <+ (t29 := src1 .+ numI32 29 32<rt>)
        bld <+ (t30 := src2 .+ numI32 30 32<rt>)
        bld <+ (t31 := src3 .+ numI32 31 32<rt>)
        bld <+ (t32 := src0 .+ numI32 32 32<rt>)
        bld <+ (t33 := src1 .+ numI32 33 32<rt>)
        bld <+ (t34 := src2 .+ numI32 34 32<rt>)
        bld <+ (t35 := src3 .+ numI32 35 32<rt>)
        bld <+ (t36 := src0 .+ numI32 36 32<rt>)
        bld <+ (t37 := src1 .+ numI32 37 32<rt>)
        bld <+ (t38 := src2 .+ numI32 38 32<rt>)
        bld <+ (t39 := src3 .+ numI32 39 32<rt>)
        bld <+ (t40 := src0 .+ numI32 40 32<rt>)
        bld <+ (t41 := src1 .+ numI32 41 32<rt>)
        bld <+ (t42 := src2 .+ numI32 42 32<rt>)
        bld <+ (t43 := src3 .+ numI32 43 32<rt>)
        bld <+ (t44 := src0 .+ numI32 44 32<rt>)
        bld <+ (t45 := src1 .+ numI32 45 32<rt>)
      | _ ->
        raise InvalidOpcodeException
"""

  /// The bar is what tells an enumeration from a branch taken in passing.
  /// `match ... with` and twenty arms come to twenty-one rows, one past the
  /// bar, so the thirty rows in front of it are not what the body is
  /// about.
  let goodMatchAtBarTest =
    """
    let liftWithSetup (ins: Instruction) insLen bld =
      bld <!-- (ins.Address, insLen)
      bld <+ (t01 := src1 .+ numI32 1 32<rt>)
      bld <+ (t02 := src2 .+ numI32 2 32<rt>)
      bld <+ (t03 := src3 .+ numI32 3 32<rt>)
      bld <+ (t04 := src0 .+ numI32 4 32<rt>)
      bld <+ (t05 := src1 .+ numI32 5 32<rt>)
      bld <+ (t06 := src2 .+ numI32 6 32<rt>)
      bld <+ (t07 := src3 .+ numI32 7 32<rt>)
      bld <+ (t08 := src0 .+ numI32 8 32<rt>)
      bld <+ (t09 := src1 .+ numI32 9 32<rt>)
      bld <+ (t10 := src2 .+ numI32 10 32<rt>)
      bld <+ (t11 := src3 .+ numI32 11 32<rt>)
      bld <+ (t12 := src0 .+ numI32 12 32<rt>)
      bld <+ (t13 := src1 .+ numI32 13 32<rt>)
      bld <+ (t14 := src2 .+ numI32 14 32<rt>)
      bld <+ (t15 := src3 .+ numI32 15 32<rt>)
      bld <+ (t16 := src0 .+ numI32 16 32<rt>)
      bld <+ (t17 := src1 .+ numI32 17 32<rt>)
      bld <+ (t18 := src2 .+ numI32 18 32<rt>)
      bld <+ (t19 := src3 .+ numI32 19 32<rt>)
      bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      bld <+ (t21 := src1 .+ numI32 21 32<rt>)
      bld <+ (t22 := src2 .+ numI32 22 32<rt>)
      bld <+ (t23 := src3 .+ numI32 23 32<rt>)
      bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      bld <+ (t25 := src1 .+ numI32 25 32<rt>)
      bld <+ (t26 := src2 .+ numI32 26 32<rt>)
      bld <+ (t27 := src3 .+ numI32 27 32<rt>)
      bld <+ (t28 := src0 .+ numI32 28 32<rt>)
      bld <+ (t29 := src1 .+ numI32 29 32<rt>)
      match ins.Opcode with
      | Op.OP01 -> lift01 ins insLen bld
      | Op.OP02 -> lift02 ins insLen bld
      | Op.OP03 -> lift03 ins insLen bld
      | Op.OP04 -> lift04 ins insLen bld
      | Op.OP05 -> lift05 ins insLen bld
      | Op.OP06 -> lift06 ins insLen bld
      | Op.OP07 -> lift07 ins insLen bld
      | Op.OP08 -> lift08 ins insLen bld
      | Op.OP09 -> lift09 ins insLen bld
      | Op.OP10 -> lift10 ins insLen bld
      | Op.OP11 -> lift11 ins insLen bld
      | Op.OP12 -> lift12 ins insLen bld
      | Op.OP13 -> lift13 ins insLen bld
      | Op.OP14 -> lift14 ins insLen bld
      | Op.OP15 -> lift15 ins insLen bld
      | Op.OP16 -> lift16 ins insLen bld
      | Op.OP17 -> lift17 ins insLen bld
      | Op.OP18 -> lift18 ins insLen bld
      | Op.OP19 -> lift19 ins insLen bld
      | Op.OP20 -> lift20 ins insLen bld
"""

  /// The same body with one arm fewer. Now the `match` is a branch taken in
  /// passing, and passing over the body for it would leave the thirty rows
  /// in front of it unmeasured for good.
  let badMatchUnderBarTest =
    """
    let liftWithSetup (ins: Instruction) insLen bld =
      bld <!-- (ins.Address, insLen)
      bld <+ (t01 := src1 .+ numI32 1 32<rt>)
      bld <+ (t02 := src2 .+ numI32 2 32<rt>)
      bld <+ (t03 := src3 .+ numI32 3 32<rt>)
      bld <+ (t04 := src0 .+ numI32 4 32<rt>)
      bld <+ (t05 := src1 .+ numI32 5 32<rt>)
      bld <+ (t06 := src2 .+ numI32 6 32<rt>)
      bld <+ (t07 := src3 .+ numI32 7 32<rt>)
      bld <+ (t08 := src0 .+ numI32 8 32<rt>)
      bld <+ (t09 := src1 .+ numI32 9 32<rt>)
      bld <+ (t10 := src2 .+ numI32 10 32<rt>)
      bld <+ (t11 := src3 .+ numI32 11 32<rt>)
      bld <+ (t12 := src0 .+ numI32 12 32<rt>)
      bld <+ (t13 := src1 .+ numI32 13 32<rt>)
      bld <+ (t14 := src2 .+ numI32 14 32<rt>)
      bld <+ (t15 := src3 .+ numI32 15 32<rt>)
      bld <+ (t16 := src0 .+ numI32 16 32<rt>)
      bld <+ (t17 := src1 .+ numI32 17 32<rt>)
      bld <+ (t18 := src2 .+ numI32 18 32<rt>)
      bld <+ (t19 := src3 .+ numI32 19 32<rt>)
      bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      bld <+ (t21 := src1 .+ numI32 21 32<rt>)
      bld <+ (t22 := src2 .+ numI32 22 32<rt>)
      bld <+ (t23 := src3 .+ numI32 23 32<rt>)
      bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      bld <+ (t25 := src1 .+ numI32 25 32<rt>)
      bld <+ (t26 := src2 .+ numI32 26 32<rt>)
      bld <+ (t27 := src3 .+ numI32 27 32<rt>)
      bld <+ (t28 := src0 .+ numI32 28 32<rt>)
      bld <+ (t29 := src1 .+ numI32 29 32<rt>)
      match ins.Opcode with
      | Op.OP01 -> lift01 ins insLen bld
      | Op.OP02 -> lift02 ins insLen bld
      | Op.OP03 -> lift03 ins insLen bld
      | Op.OP04 -> lift04 ins insLen bld
      | Op.OP05 -> lift05 ins insLen bld
      | Op.OP06 -> lift06 ins insLen bld
      | Op.OP07 -> lift07 ins insLen bld
      | Op.OP08 -> lift08 ins insLen bld
      | Op.OP09 -> lift09 ins insLen bld
      | Op.OP10 -> lift10 ins insLen bld
      | Op.OP11 -> lift11 ins insLen bld
      | Op.OP12 -> lift12 ins insLen bld
      | Op.OP13 -> lift13 ins insLen bld
      | Op.OP14 -> lift14 ins insLen bld
      | Op.OP15 -> lift15 ins insLen bld
      | Op.OP16 -> lift16 ins insLen bld
      | Op.OP17 -> lift17 ins insLen bld
      | Op.OP18 -> lift18 ins insLen bld
      | Op.OP19 -> lift19 ins insLen bld
"""

  /// Computing one field and branching on it is how a parser is written.
  /// Stopping at the `let` would leave the arms behind it counted as one
  /// stretch.
  let goodMatchBehindLetTest =
    """
    let parse (bin: uint32) =
      let field = extract bin 24u 21u
      match field with
      | 1uy -> render bin 1
      | 2uy -> render bin 2
      | 3uy -> render bin 3
      | 4uy -> render bin 4
      | 5uy -> render bin 5
      | 6uy -> render bin 6
      | 7uy -> render bin 7
      | 8uy -> render bin 8
      | 9uy -> render bin 9
      | 10uy -> render bin 10
      | 11uy -> render bin 11
      | 12uy -> render bin 12
      | 13uy -> render bin 13
      | 14uy -> render bin 14
      | 15uy -> render bin 15
      | 16uy -> render bin 16
      | 17uy -> render bin 17
      | 18uy -> render bin 18
      | 19uy -> render bin 19
      | 20uy -> render bin 20
      | 21uy -> render bin 21
      | 22uy -> render bin 22
      | 23uy -> render bin 23
      | 24uy -> render bin 24
      | 25uy -> render bin 25
      | 26uy -> render bin 26
      | 27uy -> render bin 27
      | 28uy -> render bin 28
      | 29uy -> render bin 29
      | 30uy -> render bin 30
      | 31uy -> render bin 31
      | 32uy -> render bin 32
      | 33uy -> render bin 33
      | 34uy -> render bin 34
      | 35uy -> render bin 35
      | 36uy -> render bin 36
      | 37uy -> render bin 37
      | 38uy -> render bin 38
      | 39uy -> render bin 39
      | 40uy -> render bin 40
      | 41uy -> render bin 41
      | 42uy -> render bin 42
      | 43uy -> render bin 43
      | 44uy -> render bin 44
      | 45uy -> render bin 45
      | 46uy -> render bin 46
      | 47uy -> render bin 47
      | 48uy -> render bin 48
      | 49uy -> render bin 49
      | _ -> raise ParsingFailureException
"""

  /// A statement stands in the way as readily as a `let` does. Stopping at
  /// one but not the other would split two spellings of the same shape.
  let goodMatchBehindStatementTest =
    """
    let liftAndTrace (ins: Instruction) insLen bld =
      bld <+ (t01 := src1 .+ numI32 1 32<rt>)
      bld <+ (t02 := src2 .+ numI32 2 32<rt>)
      bld <+ (t03 := src3 .+ numI32 3 32<rt>)
      bld <+ (t04 := src0 .+ numI32 4 32<rt>)
      bld <+ (t05 := src1 .+ numI32 5 32<rt>)
      bld <+ (t06 := src2 .+ numI32 6 32<rt>)
      bld <+ (t07 := src3 .+ numI32 7 32<rt>)
      bld <+ (t08 := src0 .+ numI32 8 32<rt>)
      bld <+ (t09 := src1 .+ numI32 9 32<rt>)
      bld <+ (t10 := src2 .+ numI32 10 32<rt>)
      bld <+ (t11 := src3 .+ numI32 11 32<rt>)
      bld <+ (t12 := src0 .+ numI32 12 32<rt>)
      bld <+ (t13 := src1 .+ numI32 13 32<rt>)
      bld <+ (t14 := src2 .+ numI32 14 32<rt>)
      bld <+ (t15 := src3 .+ numI32 15 32<rt>)
      bld <+ (t16 := src0 .+ numI32 16 32<rt>)
      bld <+ (t17 := src1 .+ numI32 17 32<rt>)
      bld <+ (t18 := src2 .+ numI32 18 32<rt>)
      bld <+ (t19 := src3 .+ numI32 19 32<rt>)
      bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      bld <+ (t21 := src1 .+ numI32 21 32<rt>)
      bld <+ (t22 := src2 .+ numI32 22 32<rt>)
      bld <+ (t23 := src3 .+ numI32 23 32<rt>)
      bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      bld <+ (t25 := src1 .+ numI32 25 32<rt>)
      bld <+ (t26 := src2 .+ numI32 26 32<rt>)
      bld <+ (t27 := src3 .+ numI32 27 32<rt>)
      bld <+ (t28 := src0 .+ numI32 28 32<rt>)
      bld <+ (t29 := src1 .+ numI32 29 32<rt>)
      trace ins.Opcode
      match ins.Opcode with
      | Op.OP01 -> lift01 ins insLen bld
      | Op.OP02 -> lift02 ins insLen bld
      | Op.OP03 -> lift03 ins insLen bld
      | Op.OP04 -> lift04 ins insLen bld
      | Op.OP05 -> lift05 ins insLen bld
      | Op.OP06 -> lift06 ins insLen bld
      | Op.OP07 -> lift07 ins insLen bld
      | Op.OP08 -> lift08 ins insLen bld
      | Op.OP09 -> lift09 ins insLen bld
      | Op.OP10 -> lift10 ins insLen bld
      | Op.OP11 -> lift11 ins insLen bld
      | Op.OP12 -> lift12 ins insLen bld
      | Op.OP13 -> lift13 ins insLen bld
      | Op.OP14 -> lift14 ins insLen bld
      | Op.OP15 -> lift15 ins insLen bld
      | Op.OP16 -> lift16 ins insLen bld
      | Op.OP17 -> lift17 ins insLen bld
      | Op.OP18 -> lift18 ins insLen bld
      | Op.OP19 -> lift19 ins insLen bld
      | Op.OP20 -> lift20 ins insLen bld
      | Op.OP21 -> lift21 ins insLen bld
      | Op.OP22 -> lift22 ins insLen bld
      | Op.OP23 -> lift23 ins insLen bld
      | Op.OP24 -> lift24 ins insLen bld
      | Op.OP25 -> lift25 ins insLen bld
"""

  /// A `match` whose answer is adjusted on the way out is still a `match`.
  /// `a |> f` parses as `App(App(|>, a), f)`, so what is being piped sits
  /// beside the operator rather than at the top.
  let goodMatchPipedTest =
    """
    let getConstant fimm =
      let isPositive = pick fimm 7u
      match extract fimm 6u 0u with
      | 1uy -> 1.0
      | 2uy -> 2.0
      | 3uy -> 3.0
      | 4uy -> 4.0
      | 5uy -> 5.0
      | 6uy -> 6.0
      | 7uy -> 7.0
      | 8uy -> 8.0
      | 9uy -> 9.0
      | 10uy -> 10.0
      | 11uy -> 11.0
      | 12uy -> 12.0
      | 13uy -> 13.0
      | 14uy -> 14.0
      | 15uy -> 15.0
      | 16uy -> 16.0
      | 17uy -> 17.0
      | 18uy -> 18.0
      | 19uy -> 19.0
      | 20uy -> 20.0
      | 21uy -> 21.0
      | 22uy -> 22.0
      | 23uy -> 23.0
      | 24uy -> 24.0
      | 25uy -> 25.0
      | 26uy -> 26.0
      | 27uy -> 27.0
      | 28uy -> 28.0
      | 29uy -> 29.0
      | 30uy -> 30.0
      | 31uy -> 31.0
      | 32uy -> 32.0
      | 33uy -> 33.0
      | 34uy -> 34.0
      | 35uy -> 35.0
      | 36uy -> 36.0
      | 37uy -> 37.0
      | 38uy -> 38.0
      | 39uy -> 39.0
      | 40uy -> 40.0
      | 41uy -> 41.0
      | 42uy -> 42.0
      | 43uy -> 43.0
      | 44uy -> 44.0
      | _ -> 0.0
      |> fun fp -> if isPositive then fp else -fp
"""

  /// A `match` run as one statement between two others is still what the
  /// body is about. An IR builder opens, dispatches on its operands, and
  /// closes.
  let goodMatchMidChainTest =
    """
    let fpConvert (ins: Instruction) insLen bld =
      let isNeg e = AST.xthi 1<rt> e == AST.b1
      bld <!-- (ins.Address, insLen)
      match ins.Operands with
      | TwoOperands(OprSIMD _, o01) ->
        convert01 bld isNeg o01
      | TwoOperands(OprSIMD _, o02) ->
        convert02 bld isNeg o02
      | TwoOperands(OprSIMD _, o03) ->
        convert03 bld isNeg o03
      | TwoOperands(OprSIMD _, o04) ->
        convert04 bld isNeg o04
      | TwoOperands(OprSIMD _, o05) ->
        convert05 bld isNeg o05
      | TwoOperands(OprSIMD _, o06) ->
        convert06 bld isNeg o06
      | TwoOperands(OprSIMD _, o07) ->
        convert07 bld isNeg o07
      | TwoOperands(OprSIMD _, o08) ->
        convert08 bld isNeg o08
      | TwoOperands(OprSIMD _, o09) ->
        convert09 bld isNeg o09
      | TwoOperands(OprSIMD _, o10) ->
        convert10 bld isNeg o10
      | TwoOperands(OprSIMD _, o11) ->
        convert11 bld isNeg o11
      | TwoOperands(OprSIMD _, o12) ->
        convert12 bld isNeg o12
      | TwoOperands(OprSIMD _, o13) ->
        convert13 bld isNeg o13
      | TwoOperands(OprSIMD _, o14) ->
        convert14 bld isNeg o14
      | TwoOperands(OprSIMD _, o15) ->
        convert15 bld isNeg o15
      | TwoOperands(OprSIMD _, o16) ->
        convert16 bld isNeg o16
      | TwoOperands(OprSIMD _, o17) ->
        convert17 bld isNeg o17
      | TwoOperands(OprSIMD _, o18) ->
        convert18 bld isNeg o18
      | TwoOperands(OprSIMD _, o19) ->
        convert19 bld isNeg o19
      | TwoOperands(OprSIMD _, o20) ->
        convert20 bld isNeg o20
      | TwoOperands(OprSIMD _, o21) ->
        convert21 bld isNeg o21
      | _ ->
        raise InvalidOperandException
      bld --!> insLen
"""

  /// A `while` is read the same way: one pass over a stream written out, as
  /// long as the stream has steps.
  let goodWhileLoopTest =
    """
    let parseRebase (bytes: byte[]) acc =
      let mutable cur = 0
      let mutable segOff = 0UL
      while cur < bytes.Length do
        let opcode = int bytes[cur] &&& 0xF0
        cur <- cur + 1
        segOff <- segOff + uint64 opcode + 1uL
        segOff <- segOff + uint64 opcode + 2uL
        segOff <- segOff + uint64 opcode + 3uL
        segOff <- segOff + uint64 opcode + 4uL
        segOff <- segOff + uint64 opcode + 5uL
        segOff <- segOff + uint64 opcode + 6uL
        segOff <- segOff + uint64 opcode + 7uL
        segOff <- segOff + uint64 opcode + 8uL
        segOff <- segOff + uint64 opcode + 9uL
        segOff <- segOff + uint64 opcode + 10uL
        segOff <- segOff + uint64 opcode + 11uL
        segOff <- segOff + uint64 opcode + 12uL
        segOff <- segOff + uint64 opcode + 13uL
        segOff <- segOff + uint64 opcode + 14uL
        segOff <- segOff + uint64 opcode + 15uL
        segOff <- segOff + uint64 opcode + 16uL
        segOff <- segOff + uint64 opcode + 17uL
        segOff <- segOff + uint64 opcode + 18uL
        segOff <- segOff + uint64 opcode + 19uL
        segOff <- segOff + uint64 opcode + 20uL
        segOff <- segOff + uint64 opcode + 21uL
        segOff <- segOff + uint64 opcode + 22uL
        segOff <- segOff + uint64 opcode + 23uL
        segOff <- segOff + uint64 opcode + 24uL
        segOff <- segOff + uint64 opcode + 25uL
        segOff <- segOff + uint64 opcode + 26uL
        segOff <- segOff + uint64 opcode + 27uL
        segOff <- segOff + uint64 opcode + 28uL
        segOff <- segOff + uint64 opcode + 29uL
        segOff <- segOff + uint64 opcode + 30uL
        segOff <- segOff + uint64 opcode + 31uL
        segOff <- segOff + uint64 opcode + 32uL
        segOff <- segOff + uint64 opcode + 33uL
        segOff <- segOff + uint64 opcode + 34uL
        segOff <- segOff + uint64 opcode + 35uL
        segOff <- segOff + uint64 opcode + 36uL
        segOff <- segOff + uint64 opcode + 37uL
        segOff <- segOff + uint64 opcode + 38uL
        segOff <- segOff + uint64 opcode + 39uL
      acc
"""

  /// A loop entered in passing is not. Two rows of it say nothing about the
  /// forty in front of them.
  let badShortWhileTest =
    """
    let scanForward (bytes: byte[]) start bld =
      let mutable cur = start
      bld <+ (t01 := src1 .+ numI32 1 32<rt>)
      bld <+ (t02 := src2 .+ numI32 2 32<rt>)
      bld <+ (t03 := src3 .+ numI32 3 32<rt>)
      bld <+ (t04 := src0 .+ numI32 4 32<rt>)
      bld <+ (t05 := src1 .+ numI32 5 32<rt>)
      bld <+ (t06 := src2 .+ numI32 6 32<rt>)
      bld <+ (t07 := src3 .+ numI32 7 32<rt>)
      bld <+ (t08 := src0 .+ numI32 8 32<rt>)
      bld <+ (t09 := src1 .+ numI32 9 32<rt>)
      bld <+ (t10 := src2 .+ numI32 10 32<rt>)
      bld <+ (t11 := src3 .+ numI32 11 32<rt>)
      bld <+ (t12 := src0 .+ numI32 12 32<rt>)
      bld <+ (t13 := src1 .+ numI32 13 32<rt>)
      bld <+ (t14 := src2 .+ numI32 14 32<rt>)
      bld <+ (t15 := src3 .+ numI32 15 32<rt>)
      bld <+ (t16 := src0 .+ numI32 16 32<rt>)
      bld <+ (t17 := src1 .+ numI32 17 32<rt>)
      bld <+ (t18 := src2 .+ numI32 18 32<rt>)
      bld <+ (t19 := src3 .+ numI32 19 32<rt>)
      bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      bld <+ (t21 := src1 .+ numI32 21 32<rt>)
      bld <+ (t22 := src2 .+ numI32 22 32<rt>)
      bld <+ (t23 := src3 .+ numI32 23 32<rt>)
      bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      bld <+ (t25 := src1 .+ numI32 25 32<rt>)
      bld <+ (t26 := src2 .+ numI32 26 32<rt>)
      bld <+ (t27 := src3 .+ numI32 27 32<rt>)
      bld <+ (t28 := src0 .+ numI32 28 32<rt>)
      bld <+ (t29 := src1 .+ numI32 29 32<rt>)
      bld <+ (t30 := src2 .+ numI32 30 32<rt>)
      bld <+ (t31 := src3 .+ numI32 31 32<rt>)
      bld <+ (t32 := src0 .+ numI32 32 32<rt>)
      bld <+ (t33 := src1 .+ numI32 33 32<rt>)
      bld <+ (t34 := src2 .+ numI32 34 32<rt>)
      bld <+ (t35 := src3 .+ numI32 35 32<rt>)
      bld <+ (t36 := src0 .+ numI32 36 32<rt>)
      bld <+ (t37 := src1 .+ numI32 37 32<rt>)
      bld <+ (t38 := src2 .+ numI32 38 32<rt>)
      bld <+ (t39 := src3 .+ numI32 39 32<rt>)
      while cur < bytes.Length do
        cur <- cur + 1
      cur
"""

  /// Either branch of a conditional is reached. A parser that answers one
  /// way at the end of its input and enumerates opcodes otherwise is an
  /// enumeration, and the guard in front of it does not make it something
  /// else.
  let goodConditionalBranchTest =
    """
    let rec parse (span: ByteSpan) exprs i maxIdx =
      if i >= maxIdx then
        match exprs with
        | [ exp ] -> exp
        | _ -> raise InvalidExpressionException
      else
        match DWOperation.parse span[i] with
        | DWOperation.OP01 ->
          parse span exprs (i + 1) maxIdx
        | DWOperation.OP02 ->
          parse span exprs (i + 2) maxIdx
        | DWOperation.OP03 ->
          parse span exprs (i + 3) maxIdx
        | DWOperation.OP04 ->
          parse span exprs (i + 4) maxIdx
        | DWOperation.OP05 ->
          parse span exprs (i + 5) maxIdx
        | DWOperation.OP06 ->
          parse span exprs (i + 6) maxIdx
        | DWOperation.OP07 ->
          parse span exprs (i + 7) maxIdx
        | DWOperation.OP08 ->
          parse span exprs (i + 8) maxIdx
        | DWOperation.OP09 ->
          parse span exprs (i + 9) maxIdx
        | DWOperation.OP10 ->
          parse span exprs (i + 10) maxIdx
        | DWOperation.OP11 ->
          parse span exprs (i + 11) maxIdx
        | DWOperation.OP12 ->
          parse span exprs (i + 12) maxIdx
        | DWOperation.OP13 ->
          parse span exprs (i + 13) maxIdx
        | DWOperation.OP14 ->
          parse span exprs (i + 14) maxIdx
        | DWOperation.OP15 ->
          parse span exprs (i + 15) maxIdx
        | DWOperation.OP16 ->
          parse span exprs (i + 16) maxIdx
        | DWOperation.OP17 ->
          parse span exprs (i + 17) maxIdx
        | DWOperation.OP18 ->
          parse span exprs (i + 18) maxIdx
        | DWOperation.OP19 ->
          parse span exprs (i + 19) maxIdx
        | DWOperation.OP20 ->
          parse span exprs (i + 20) maxIdx
        | _ ->
          raise InvalidExpressionException
"""

  /// With plain code down both branches the body can run to its end without
  /// enumerating anything, and that stretch is what the budget is for.
  let badNeitherBranchTest =
    """
    let liftEither (ins: Instruction) insLen bld isWide =
      if isWide then
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      else
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
        bld <+ (t25 := src1 .+ numI32 25 32<rt>)
        bld <+ (t26 := src2 .+ numI32 26 32<rt>)
        bld <+ (t27 := src3 .+ numI32 27 32<rt>)
        bld <+ (t28 := src0 .+ numI32 28 32<rt>)
        bld <+ (t29 := src1 .+ numI32 29 32<rt>)
        bld <+ (t30 := src2 .+ numI32 30 32<rt>)
        bld <+ (t31 := src3 .+ numI32 31 32<rt>)
        bld <+ (t32 := src0 .+ numI32 32 32<rt>)
        bld <+ (t33 := src1 .+ numI32 33 32<rt>)
        bld <+ (t34 := src2 .+ numI32 34 32<rt>)
        bld <+ (t35 := src3 .+ numI32 35 32<rt>)
        bld <+ (t36 := src0 .+ numI32 36 32<rt>)
        bld <+ (t37 := src1 .+ numI32 37 32<rt>)
        bld <+ (t38 := src2 .+ numI32 38 32<rt>)
        bld <+ (t39 := src3 .+ numI32 39 32<rt>)
        bld <+ (t40 := src0 .+ numI32 40 32<rt>)
        bld <+ (t41 := src1 .+ numI32 41 32<rt>)
        bld <+ (t42 := src2 .+ numI32 42 32<rt>)
"""

  /// Which of two tables to hand back is not the branching this rule is
  /// here to measure.
  let goodConditionalTableTest =
    """
    let registers is32 =
      if is32 then
        [| Register.E00
           Register.E01
           Register.E02
           Register.E03
           Register.E04
           Register.E05
           Register.E06
           Register.E07
           Register.E08
           Register.E09
           Register.E10
           Register.E11
           Register.E12
           Register.E13
           Register.E14
           Register.E15
           Register.E16
           Register.E17
           Register.E18
           Register.E19
           Register.E20
           Register.E21
           Register.E22
           Register.E23
           Register.E24 |]
      else
        [| Register.R00
           Register.R01
           Register.R02
           Register.R03
           Register.R04
           Register.R05
           Register.R06
           Register.R07
           Register.R08
           Register.R09
           Register.R10
           Register.R11
           Register.R12
           Register.R13
           Register.R14
           Register.R15
           Register.R16
           Register.R17
           Register.R18
           Register.R19
           Register.R20
           Register.R21
           Register.R22
           Register.R23
           Register.R24 |]
"""

  /// A helper written inside the body is the body's own rows, so a long
  /// `match` inside one answers for it. Splitting the caller would only
  /// move the helper, which is already split.
  let goodInnerBindingTest =
    """
    let parseModule bs reader off =
      let version = peekFormatVersion bs reader off
      let summary = summarizeSections bs reader off
      if not (validateSectionsOrder summary) then
        raise InvalidFileFormatException
      else
        let rec parsingLoop wasmModule summary =
          match (List.head summary).Id with
          | SectionId.S01 ->
            parsingLoop wasmModule 1
          | SectionId.S02 ->
            parsingLoop wasmModule 2
          | SectionId.S03 ->
            parsingLoop wasmModule 3
          | SectionId.S04 ->
            parsingLoop wasmModule 4
          | SectionId.S05 ->
            parsingLoop wasmModule 5
          | SectionId.S06 ->
            parsingLoop wasmModule 6
          | SectionId.S07 ->
            parsingLoop wasmModule 7
          | SectionId.S08 ->
            parsingLoop wasmModule 8
          | SectionId.S09 ->
            parsingLoop wasmModule 9
          | SectionId.S10 ->
            parsingLoop wasmModule 10
          | SectionId.S11 ->
            parsingLoop wasmModule 11
          | SectionId.S12 ->
            parsingLoop wasmModule 12
          | SectionId.S13 ->
            parsingLoop wasmModule 13
          | SectionId.S14 ->
            parsingLoop wasmModule 14
          | SectionId.S15 ->
            parsingLoop wasmModule 15
          | SectionId.S16 ->
            parsingLoop wasmModule 16
          | SectionId.S17 ->
            parsingLoop wasmModule 17
          | SectionId.S18 ->
            parsingLoop wasmModule 18
          | SectionId.S19 ->
            parsingLoop wasmModule 19
          | SectionId.S20 ->
            parsingLoop wasmModule 20
          | _ ->
            wasmModule
        let wasmModule = { Version = version; Sections = [] }
        parsingLoop wasmModule summary
"""

  /// A binding written inside another is passed over. Its rows are already
  /// counted in the body holding it, and the two are not two lengths but
  /// one; the outermost is where the splitting has to start. Here that one
  /// reaches a long `match`, so nothing is asked of either.
  let goodNestedBindingTest =
    """
    let liftGroup (ins: Instruction) insLen bld =
      let liftOne src1 src2 src3 =
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      match ins.Opcode with
      | Op.OP01 -> lift01 ins insLen bld
      | Op.OP02 -> lift02 ins insLen bld
      | Op.OP03 -> lift03 ins insLen bld
      | Op.OP04 -> lift04 ins insLen bld
      | Op.OP05 -> lift05 ins insLen bld
      | Op.OP06 -> lift06 ins insLen bld
      | Op.OP07 -> lift07 ins insLen bld
      | Op.OP08 -> lift08 ins insLen bld
      | Op.OP09 -> lift09 ins insLen bld
      | Op.OP10 -> lift10 ins insLen bld
      | Op.OP11 -> lift11 ins insLen bld
      | Op.OP12 -> lift12 ins insLen bld
      | Op.OP13 -> lift13 ins insLen bld
      | Op.OP14 -> lift14 ins insLen bld
      | Op.OP15 -> lift15 ins insLen bld
      | Op.OP16 -> lift16 ins insLen bld
      | Op.OP17 -> lift17 ins insLen bld
      | Op.OP18 -> lift18 ins insLen bld
      | Op.OP19 -> lift19 ins insLen bld
      | Op.OP20 -> lift20 ins insLen bld
      | Op.OP21 -> lift21 ins insLen bld
      | Op.OP22 -> lift22 ins insLen bld
      | Op.OP23 -> lift23 ins insLen bld
      | Op.OP24 -> lift24 ins insLen bld
      | Op.OP25 -> lift25 ins insLen bld
"""

  /// A `let` naming something the table uses does not stop the body being a
  /// table, and a table is passed over without being noted as covered. So
  /// the name it binds is still reached and still answers for its own
  /// length -- that one can be lifted out where the table cannot.
  let badTableHelperTest =
    """
    let advancedSIMD () =
      let shiftWith src1 src2 src3 bld =
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
        bld <+ (t25 := src1 .+ numI32 25 32<rt>)
        bld <+ (t26 := src2 .+ numI32 26 32<rt>)
        bld <+ (t27 := src3 .+ numI32 27 32<rt>)
        bld <+ (t28 := src0 .+ numI32 28 32<rt>)
        bld <+ (t29 := src1 .+ numI32 29 32<rt>)
        bld <+ (t30 := src2 .+ numI32 30 32<rt>)
        bld <+ (t31 := src3 .+ numI32 31 32<rt>)
        bld <+ (t32 := src0 .+ numI32 32 32<rt>)
        bld <+ (t33 := src1 .+ numI32 33 32<rt>)
        bld <+ (t34 := src2 .+ numI32 34 32<rt>)
        bld <+ (t35 := src3 .+ numI32 35 32<rt>)
        bld <+ (t36 := src0 .+ numI32 36 32<rt>)
        bld <+ (t37 := src1 .+ numI32 37 32<rt>)
        bld <+ (t38 := src2 .+ numI32 38 32<rt>)
        bld <+ (t39 := src3 .+ numI32 39 32<rt>)
        bld <+ (t40 := src0 .+ numI32 40 32<rt>)
        bld <+ (t41 := src1 .+ numI32 41 32<rt>)
        bld <+ (t42 := src2 .+ numI32 42 32<rt>)
        bld <+ (t43 := src3 .+ numI32 43 32<rt>)
        bld <+ (t44 := src0 .+ numI32 44 32<rt>)
      [ 0, shiftWith
        1, shiftWith ]
"""

  /// What stands inside a lambda, or beside a call as its argument, is not
  /// reached. Searching there would turn up a `match` in almost every body
  /// and leave nothing measured at all.
  let badLambdaMatchTest =
    """
    let summarize probes bld src1 src2 src3 =
      bld <+ (t01 := src1 .+ numI32 1 32<rt>)
      bld <+ (t02 := src2 .+ numI32 2 32<rt>)
      bld <+ (t03 := src3 .+ numI32 3 32<rt>)
      bld <+ (t04 := src0 .+ numI32 4 32<rt>)
      bld <+ (t05 := src1 .+ numI32 5 32<rt>)
      bld <+ (t06 := src2 .+ numI32 6 32<rt>)
      bld <+ (t07 := src3 .+ numI32 7 32<rt>)
      bld <+ (t08 := src0 .+ numI32 8 32<rt>)
      bld <+ (t09 := src1 .+ numI32 9 32<rt>)
      bld <+ (t10 := src2 .+ numI32 10 32<rt>)
      bld <+ (t11 := src3 .+ numI32 11 32<rt>)
      bld <+ (t12 := src0 .+ numI32 12 32<rt>)
      bld <+ (t13 := src1 .+ numI32 13 32<rt>)
      bld <+ (t14 := src2 .+ numI32 14 32<rt>)
      bld <+ (t15 := src3 .+ numI32 15 32<rt>)
      bld <+ (t16 := src0 .+ numI32 16 32<rt>)
      bld <+ (t17 := src1 .+ numI32 17 32<rt>)
      bld <+ (t18 := src2 .+ numI32 18 32<rt>)
      bld <+ (t19 := src3 .+ numI32 19 32<rt>)
      bld <+ (t20 := src0 .+ numI32 20 32<rt>)
      bld <+ (t21 := src1 .+ numI32 21 32<rt>)
      bld <+ (t22 := src2 .+ numI32 22 32<rt>)
      bld <+ (t23 := src3 .+ numI32 23 32<rt>)
      bld <+ (t24 := src0 .+ numI32 24 32<rt>)
      bld <+ (t25 := src1 .+ numI32 25 32<rt>)
      bld <+ (t26 := src2 .+ numI32 26 32<rt>)
      bld <+ (t27 := src3 .+ numI32 27 32<rt>)
      bld <+ (t28 := src0 .+ numI32 28 32<rt>)
      bld <+ (t29 := src1 .+ numI32 29 32<rt>)
      bld <+ (t30 := src2 .+ numI32 30 32<rt>)
      bld <+ (t31 := src3 .+ numI32 31 32<rt>)
      bld <+ (t32 := src0 .+ numI32 32 32<rt>)
      bld <+ (t33 := src1 .+ numI32 33 32<rt>)
      bld <+ (t34 := src2 .+ numI32 34 32<rt>)
      bld <+ (t35 := src3 .+ numI32 35 32<rt>)
      bld <+ (t36 := src0 .+ numI32 36 32<rt>)
      bld <+ (t37 := src1 .+ numI32 37 32<rt>)
      bld <+ (t38 := src2 .+ numI32 38 32<rt>)
      bld <+ (t39 := src3 .+ numI32 39 32<rt>)
      bld <+ (t40 := src0 .+ numI32 40 32<rt>)
      probes
      |> List.map (fun probe ->
        match probe.Opcode with
        | Op.OP01 -> 1
        | Op.OP02 -> 2
        | Op.OP03 -> 3
        | Op.OP04 -> 4
        | Op.OP05 -> 5
        | Op.OP06 -> 6
        | Op.OP07 -> 7
        | Op.OP08 -> 8
        | Op.OP09 -> 9
        | Op.OP10 -> 10
        | Op.OP11 -> 11
        | Op.OP12 -> 12
        | Op.OP13 -> 13
        | Op.OP14 -> 14
        | Op.OP15 -> 15
        | Op.OP16 -> 16
        | Op.OP17 -> 17
        | Op.OP18 -> 18
        | Op.OP19 -> 19
        | Op.OP20 -> 20
        | Op.OP21 -> 21
        | Op.OP22 -> 22
        | Op.OP23 -> 23
        | Op.OP24 -> 24
        | _ -> 0)
"""

  /// A test is a scenario: setting up, running and checking, in that order.
  /// Cutting it in three gives three names that mean nothing outside the
  /// test they came from.
  let goodTestMethodTest =
    """
    type ParserTests() =

      [<TestMethod>]
      member _.``Signed Modulo``() =
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
        bld <+ (t25 := src1 .+ numI32 25 32<rt>)
        bld <+ (t26 := src2 .+ numI32 26 32<rt>)
        bld <+ (t27 := src3 .+ numI32 27 32<rt>)
        bld <+ (t28 := src0 .+ numI32 28 32<rt>)
        bld <+ (t29 := src1 .+ numI32 29 32<rt>)
        bld <+ (t30 := src2 .+ numI32 30 32<rt>)
        bld <+ (t31 := src3 .+ numI32 31 32<rt>)
        bld <+ (t32 := src0 .+ numI32 32 32<rt>)
        bld <+ (t33 := src1 .+ numI32 33 32<rt>)
        bld <+ (t34 := src2 .+ numI32 34 32<rt>)
        bld <+ (t35 := src3 .+ numI32 35 32<rt>)
        bld <+ (t36 := src0 .+ numI32 36 32<rt>)
        bld <+ (t37 := src1 .+ numI32 37 32<rt>)
        bld <+ (t38 := src2 .+ numI32 38 32<rt>)
        bld <+ (t39 := src3 .+ numI32 39 32<rt>)
        bld <+ (t40 := src0 .+ numI32 40 32<rt>)
        bld <+ (t41 := src1 .+ numI32 41 32<rt>)
        bld <+ (t42 := src2 .+ numI32 42 32<rt>)
        bld <+ (t43 := src3 .+ numI32 43 32<rt>)
        bld <+ (t44 := src0 .+ numI32 44 32<rt>)
        bld <+ (t45 := src1 .+ numI32 45 32<rt>)
        Assert.AreEqual<int>(1, t01)
"""

  /// A member without one is a member like any other.
  let badPlainMemberTest =
    """
    type Parser() =

      member _.Run(bld, src1, src2, src3) =
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
        bld <+ (t25 := src1 .+ numI32 25 32<rt>)
        bld <+ (t26 := src2 .+ numI32 26 32<rt>)
        bld <+ (t27 := src3 .+ numI32 27 32<rt>)
        bld <+ (t28 := src0 .+ numI32 28 32<rt>)
        bld <+ (t29 := src1 .+ numI32 29 32<rt>)
        bld <+ (t30 := src2 .+ numI32 30 32<rt>)
        bld <+ (t31 := src3 .+ numI32 31 32<rt>)
        bld <+ (t32 := src0 .+ numI32 32 32<rt>)
        bld <+ (t33 := src1 .+ numI32 33 32<rt>)
        bld <+ (t34 := src2 .+ numI32 34 32<rt>)
        bld <+ (t35 := src3 .+ numI32 35 32<rt>)
        bld <+ (t36 := src0 .+ numI32 36 32<rt>)
        bld <+ (t37 := src1 .+ numI32 37 32<rt>)
        bld <+ (t38 := src2 .+ numI32 38 32<rt>)
        bld <+ (t39 := src3 .+ numI32 39 32<rt>)
        bld <+ (t40 := src0 .+ numI32 40 32<rt>)
        bld <+ (t41 := src1 .+ numI32 41 32<rt>)
        bld <+ (t42 := src2 .+ numI32 42 32<rt>)
        bld <+ (t43 := src3 .+ numI32 43 32<rt>)
        bld <+ (t44 := src0 .+ numI32 44 32<rt>)
        bld <+ (t45 := src1 .+ numI32 45 32<rt>)
        t01
"""

  /// An object expression is a type written where an expression stands. The
  /// line assembling it is as long as it has members, which is an
  /// enumeration; each member answers for its own body, as a class member
  /// does. So the report names the member, not the binding around it.
  let goodObjectExpressionTest =
    """
    let makeLifter bld src1 src2 src3 =
      { new ILiftable with
          member _.Lift(ins, insLen) =
            bld <+ (t01 := src1 .+ numI32 1 32<rt>)
            bld <+ (t02 := src2 .+ numI32 2 32<rt>)
            bld <+ (t03 := src3 .+ numI32 3 32<rt>)
            bld <+ (t04 := src0 .+ numI32 4 32<rt>)
            bld <+ (t05 := src1 .+ numI32 5 32<rt>)
            bld <+ (t06 := src2 .+ numI32 6 32<rt>)
            bld <+ (t07 := src3 .+ numI32 7 32<rt>)
            bld <+ (t08 := src0 .+ numI32 8 32<rt>)
            bld <+ (t09 := src1 .+ numI32 9 32<rt>)
            bld <+ (t10 := src2 .+ numI32 10 32<rt>)
            bld <+ (t11 := src3 .+ numI32 11 32<rt>)
            bld <+ (t12 := src0 .+ numI32 12 32<rt>)
            bld <+ (t13 := src1 .+ numI32 13 32<rt>)
            bld <+ (t14 := src2 .+ numI32 14 32<rt>)
            bld <+ (t15 := src3 .+ numI32 15 32<rt>)
            bld <+ (t16 := src0 .+ numI32 16 32<rt>)
            bld <+ (t17 := src1 .+ numI32 17 32<rt>)
            bld <+ (t18 := src2 .+ numI32 18 32<rt>)
            bld <+ (t19 := src3 .+ numI32 19 32<rt>)
            bld <+ (t20 := src0 .+ numI32 20 32<rt>)
            bld <+ (t21 := src1 .+ numI32 21 32<rt>)
            bld <+ (t22 := src2 .+ numI32 22 32<rt>)
            bld <+ (t23 := src3 .+ numI32 23 32<rt>)
            bld <+ (t24 := src0 .+ numI32 24 32<rt>)
            bld <+ (t25 := src1 .+ numI32 25 32<rt>)
            bld <+ (t26 := src2 .+ numI32 26 32<rt>)
            bld <+ (t27 := src3 .+ numI32 27 32<rt>)
            bld <+ (t28 := src0 .+ numI32 28 32<rt>)
            bld <+ (t29 := src1 .+ numI32 29 32<rt>)
            bld <+ (t30 := src2 .+ numI32 30 32<rt>)
            bld <+ (t31 := src3 .+ numI32 31 32<rt>)
            bld <+ (t32 := src0 .+ numI32 32 32<rt>)
            bld <+ (t33 := src1 .+ numI32 33 32<rt>)
            bld <+ (t34 := src2 .+ numI32 34 32<rt>)
            bld <+ (t35 := src3 .+ numI32 35 32<rt>)
            bld <+ (t36 := src0 .+ numI32 36 32<rt>)
            bld <+ (t37 := src1 .+ numI32 37 32<rt>)
            bld <+ (t38 := src2 .+ numI32 38 32<rt>)
            bld <+ (t39 := src3 .+ numI32 39 32<rt>)
            bld <+ (t40 := src0 .+ numI32 40 32<rt>)
            bld <+ (t41 := src1 .+ numI32 41 32<rt>)
            bld <+ (t42 := src2 .+ numI32 42 32<rt>)
            bld <+ (t43 := src3 .+ numI32 43 32<rt>)
            bld <+ (t44 := src0 .+ numI32 44 32<rt>)
            bld --!> insLen }
"""

  /// `try ... with` divides the way a `match` does, and its clauses answer
  /// for nothing. What is guarded is measured on its own.
  let goodTryBodyTest =
    """
    let tryLift (ins: Instruction) insLen bld src1 src2 src3 =
      try
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
        bld <+ (t25 := src1 .+ numI32 25 32<rt>)
        bld <+ (t26 := src2 .+ numI32 26 32<rt>)
        bld <+ (t27 := src3 .+ numI32 27 32<rt>)
        bld <+ (t28 := src0 .+ numI32 28 32<rt>)
        bld <+ (t29 := src1 .+ numI32 29 32<rt>)
        bld <+ (t30 := src2 .+ numI32 30 32<rt>)
        bld <+ (t31 := src3 .+ numI32 31 32<rt>)
        bld <+ (t32 := src0 .+ numI32 32 32<rt>)
        bld <+ (t33 := src1 .+ numI32 33 32<rt>)
        bld <+ (t34 := src2 .+ numI32 34 32<rt>)
        bld <+ (t35 := src3 .+ numI32 35 32<rt>)
        bld <+ (t36 := src0 .+ numI32 36 32<rt>)
        bld <+ (t37 := src1 .+ numI32 37 32<rt>)
        bld <+ (t38 := src2 .+ numI32 38 32<rt>)
        bld <+ (t39 := src3 .+ numI32 39 32<rt>)
        bld <+ (t40 := src0 .+ numI32 40 32<rt>)
      with
      | :? InvalidOperandException -> ()
      | _ -> reraise ()
"""

  /// Three rows more inside the guarded body, and it answers for its length
  /// the way any other stretch would.
  let badTryBodyTest =
    """
    let tryLift (ins: Instruction) insLen bld src1 src2 src3 =
      try
        bld <+ (t01 := src1 .+ numI32 1 32<rt>)
        bld <+ (t02 := src2 .+ numI32 2 32<rt>)
        bld <+ (t03 := src3 .+ numI32 3 32<rt>)
        bld <+ (t04 := src0 .+ numI32 4 32<rt>)
        bld <+ (t05 := src1 .+ numI32 5 32<rt>)
        bld <+ (t06 := src2 .+ numI32 6 32<rt>)
        bld <+ (t07 := src3 .+ numI32 7 32<rt>)
        bld <+ (t08 := src0 .+ numI32 8 32<rt>)
        bld <+ (t09 := src1 .+ numI32 9 32<rt>)
        bld <+ (t10 := src2 .+ numI32 10 32<rt>)
        bld <+ (t11 := src3 .+ numI32 11 32<rt>)
        bld <+ (t12 := src0 .+ numI32 12 32<rt>)
        bld <+ (t13 := src1 .+ numI32 13 32<rt>)
        bld <+ (t14 := src2 .+ numI32 14 32<rt>)
        bld <+ (t15 := src3 .+ numI32 15 32<rt>)
        bld <+ (t16 := src0 .+ numI32 16 32<rt>)
        bld <+ (t17 := src1 .+ numI32 17 32<rt>)
        bld <+ (t18 := src2 .+ numI32 18 32<rt>)
        bld <+ (t19 := src3 .+ numI32 19 32<rt>)
        bld <+ (t20 := src0 .+ numI32 20 32<rt>)
        bld <+ (t21 := src1 .+ numI32 21 32<rt>)
        bld <+ (t22 := src2 .+ numI32 22 32<rt>)
        bld <+ (t23 := src3 .+ numI32 23 32<rt>)
        bld <+ (t24 := src0 .+ numI32 24 32<rt>)
        bld <+ (t25 := src1 .+ numI32 25 32<rt>)
        bld <+ (t26 := src2 .+ numI32 26 32<rt>)
        bld <+ (t27 := src3 .+ numI32 27 32<rt>)
        bld <+ (t28 := src0 .+ numI32 28 32<rt>)
        bld <+ (t29 := src1 .+ numI32 29 32<rt>)
        bld <+ (t30 := src2 .+ numI32 30 32<rt>)
        bld <+ (t31 := src3 .+ numI32 31 32<rt>)
        bld <+ (t32 := src0 .+ numI32 32 32<rt>)
        bld <+ (t33 := src1 .+ numI32 33 32<rt>)
        bld <+ (t34 := src2 .+ numI32 34 32<rt>)
        bld <+ (t35 := src3 .+ numI32 35 32<rt>)
        bld <+ (t36 := src0 .+ numI32 36 32<rt>)
        bld <+ (t37 := src1 .+ numI32 37 32<rt>)
        bld <+ (t38 := src2 .+ numI32 38 32<rt>)
        bld <+ (t39 := src3 .+ numI32 39 32<rt>)
        bld <+ (t40 := src0 .+ numI32 40 32<rt>)
        bld <+ (t41 := src1 .+ numI32 41 32<rt>)
        bld <+ (t42 := src2 .+ numI32 42 32<rt>)
        bld <+ (t43 := src3 .+ numI32 43 32<rt>)
      with
      | :? InvalidOperandException -> ()
      | _ -> reraise ()
"""

  /// The budget is measured to the row, and both sides of it are checked.
  [<TestMethod>]
  member _.``[RowLength] Budget Test``() =
    lint goodBudgetTest
    lintAssertMsg "Split into smaller functions" badOverBudgetTest

  /// A body that is one piece of data written out is passed over however far
  /// down the page it runs.
  [<TestMethod>]
  member _.``[RowLength] Data Table Test``() =
    lint goodArrayTableTest
    lint goodRecordTableTest
    lint goodStringTableTest
    lint goodPipedTableTest

  /// A literal reached through what shapes it is still the literal being read,
  /// but only where the call takes the one argument beside it.
  [<TestMethod>]
  member _.``[RowLength] Shaped Table Test``() =
    lint goodShapedTableTest
    lintAssertMsg "Split into smaller functions" badTwoArgumentTableTest

  /// Which brackets were typed says nothing about what stands in them; what
  /// the builder does with them says everything.
  [<TestMethod>]
  member _.``[RowLength] Computation Expression Test``() =
    lint goodSeqTableTest
    lintAssertMsg "Split into smaller functions" badAsyncWorkTest

  /// Neither the arms of a long `match` nor what stands under them is
  /// measured.
  [<TestMethod>]
  member _.``[RowLength] Long Match Test``() =
    lint goodLongMatchTest
    lint goodFatClauseTest

  /// Twenty rows tells an enumeration from a branch taken in passing, and the
  /// two samples differ by one arm.
  [<TestMethod>]
  member _.``[RowLength] Enumeration Budget Test``() =
    lint goodMatchAtBarTest
    lintAssertMsg "Split into smaller functions" badMatchUnderBarTest

  /// However the enumeration was threaded in, it is what the body is about.
  [<TestMethod>]
  member _.``[RowLength] Threaded Enumeration Test``() =
    lint goodMatchBehindLetTest
    lint goodMatchBehindStatementTest
    lint goodMatchPipedTest
    lint goodMatchMidChainTest

  /// A `while` takes its length from how many steps the stream has.
  [<TestMethod>]
  member _.``[RowLength] While Loop Test``() =
    lint goodWhileLoopTest
    lintAssertMsg "Split into smaller functions" badShortWhileTest

  /// Either branch of a conditional is reached, and a table is a table only
  /// where both branches are.
  [<TestMethod>]
  member _.``[RowLength] Conditional Test``() =
    lint goodConditionalBranchTest
    lint goodConditionalTableTest
    lintAssertMsg "Split into smaller functions" badNeitherBranchTest

  /// A helper written inside the body is the body's own rows, so a long
  /// `match` inside one answers for it.
  [<TestMethod>]
  member _.``[RowLength] Inner Binding Test``() =
    lint goodInnerBindingTest
    lint goodNestedBindingTest

  /// A table is passed over without being noted as covered, so a helper named
  /// in front of one is still reached and still answers for itself.
  [<TestMethod>]
  member _.``[RowLength] Table Helper Test``() =
    lintAssertMsg "Split into smaller functions" badTableHelperTest

  /// What stands inside a lambda, or beside a call as its argument, is not
  /// reached.
  [<TestMethod>]
  member _.``[RowLength] Off The Spine Test``() =
    lintAssertMsg "Split into smaller functions" badLambdaMatchTest

  /// A test is a scenario, and cutting it in three gives three names that mean
  /// nothing outside it. Without the attribute it is a member like any other.
  [<TestMethod>]
  member _.``[RowLength] Test Method Test``() =
    lint goodTestMethodTest
    lintAssertMsg "Split into smaller functions" badPlainMemberTest

  /// What is guarded is measured on its own, and the clauses answer for
  /// nothing.
  [<TestMethod>]
  member _.``[RowLength] Try Body Test``() =
    lint goodTryBodyTest
    lintAssertMsg "Split into smaller functions" badTryBodyTest

  /// Each member of an object expression answers for its own body, and the
  /// report names the member rather than the binding assembling it.
  [<TestMethod>]
  member _.``[RowLength] Object Expression Test``() =
    lintErrors goodObjectExpressionTest
    |> List.filter (fun e -> e.Message = "Split into smaller functions")
    |> fun errors ->
      Assert.AreEqual<int>(1, errors.Length)
      Assert.AreEqual<int>(4, errors.Head.Range.StartLine)
