package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGenParser
import fastparse.core.Parsed.{Failure, Success}
import org.scalacheck.Gen
import org.scalatest.exceptions.TestFailedException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.{Base58 => ScorexBase58}

class ParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGenParser with NoShrink {

  private def parseOne(x: String): EXPR = Parser(x) match {
    case Success(r, _) =>
      if (r.size > 1) {
        println(s"Can't parse (len=${x.length}): <START>\n$x\n<END>")
        throw new TestFailedException(s"Expected 1 expression, but got ${r.size}: $r", 0)
      } else r.head
    case e @ Failure(_, i, _) =>
      println(
        s"Can't parse (len=${x.length}): <START>\n$x\n<END>\nError: $e\nPosition ($i): '${x.slice(i, i + 1)}'\nTraced:\n${e.extra.traced.fullStack
          .mkString("\n")}")
      throw new TestFailedException("Test failed", 0)
  }

  private def parseAll(x: String): Seq[EXPR] = Parser(x) match {
    case Success(r, _) => r
    case e @ Failure(_, i, _) =>
      println(x)
      println(
        s"Can't parse (len=${x.length}): <START>\n$x\n<END>\nError: $e\nPosition ($i): '${x.slice(i, i + 1)}'\nTraced:\n${e.extra.traced.fullStack
          .mkString("\n")}")
      throw new TestFailedException("Test failed", 0)
  }

  private def isParsed(x: String): Boolean = Parser(x) match {
    case Success(_, _)    => true
    case Failure(_, _, _) => false
  }

  private def genElementCheck(gen: Gen[EXPR]): Unit = {
    val testGen: Gen[(EXPR, String)] = for {
      expr <- gen
      str  <- toString(expr)
    } yield (expr, str)

    forAll(testGen) {
      case (expr, str) =>
        withClue(str) {
          parseOne(str) shouldBe expr
        }
    }
  }

  private def multiLineExprTests(tests: (String, Gen[EXPR])*): Unit = tests.foreach {
    case (label, gen) =>
      property(s"multiline expressions: $label") {
        genElementCheck(gen)
      }
  }

  private val gas = 50
  multiLineExprTests(
    "CONST_LONG" -> CONST_LONGgen.map(_._1),
    "STR"        -> STRgen,
    "REF"        -> REFgen,
    "BOOL"       -> BOOLgen(gas).map(_._1),
    "SUM"        -> SUMgen(gas).map(_._1),
    "EQ"         -> EQ_INTgen(gas).map(_._1),
    "INT"        -> INTGen(gas).map(_._1),
    "GE"         -> GEgen(gas).map(_._1),
    "GT"         -> GTgen(gas).map(_._1),
    "AND"        -> ANDgen(gas).map(_._1),
    "OR"         -> ORgen(gas).map(_._1),
    "BLOCK"      -> BLOCKgen(gas)
  )

  property("priority in binary expressions") {
    parseOne("1 == 0 || 3 == 2") shouldBe BINARY_OP(
      0,
      16,
      BINARY_OP(0, 6, CONST_LONG(0, 1, 1), EQ_OP, CONST_LONG(5, 6, 0)),
      OR_OP,
      BINARY_OP(10, 16, CONST_LONG(10, 11, 3), EQ_OP, CONST_LONG(15, 16, 2))
    )
    parseOne("3 + 2 > 2 + 1") shouldBe BINARY_OP(
      0,
      13,
      BINARY_OP(0, 5, CONST_LONG(0, 1, 3), SUM_OP, CONST_LONG(4, 5, 2)),
      GT_OP,
      BINARY_OP(8, 13, CONST_LONG(8, 9, 2), SUM_OP, CONST_LONG(12, 13, 1))
    )
    parseOne("1 >= 0 || 3 > 2") shouldBe BINARY_OP(
      0,
      15,
      BINARY_OP(0, 6, CONST_LONG(0, 1, 1), GE_OP, CONST_LONG(5, 6, 0)),
      OR_OP,
      BINARY_OP(10, 15, CONST_LONG(10, 11, 3), GT_OP, CONST_LONG(14, 15, 2))
    )
  }

  property("bytestr expressions") {
    parseOne("false || sigVerify(base58'333', base58'222', base58'111')") shouldBe BINARY_OP(
      0,
      57,
      FALSE(0, 5),
      OR_OP,
      FUNCTION_CALL(
        9,
        57,
        PART.VALID(9, 18, "sigVerify"),
        List(
          CONST_BYTEVECTOR(19, 30, PART.VALID(27, 29, ByteVector(ScorexBase58.decode("333").get))),
          CONST_BYTEVECTOR(32, 43, PART.VALID(40, 42, ByteVector(ScorexBase58.decode("222").get))),
          CONST_BYTEVECTOR(45, 56, PART.VALID(53, 55, ByteVector(ScorexBase58.decode("111").get)))
        )
      )
    )
  }

  property("valid non-empty base58 definition") {
    parseOne("base58'bQbp'") shouldBe CONST_BYTEVECTOR(0, 12, PART.VALID(8, 11, ByteVector("foo".getBytes)))
  }

  property("valid empty base58 definition") {
    parseOne("base58''") shouldBe CONST_BYTEVECTOR(0, 8, PART.VALID(8, 7, ByteVector.empty))
  }

  property("invalid base58 definition") {
    parseOne("base58' bQbp'") shouldBe CONST_BYTEVECTOR(0, 13, PART.INVALID(8, 12, "can't parse Base58 string"))
  }

  property("string is consumed fully") {
    parseOne(""" "   fooo    bar" """) shouldBe CONST_STRING(1, 17, PART.VALID(2, 16, "   fooo    bar"))
  }

  property("string literal with unicode chars") {
    val stringWithUnicodeChars = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD"

    parseOne(
      s"""
         |
         | "$stringWithUnicodeChars"
         |
       """.stripMargin
    ) shouldBe CONST_STRING(3, 20, PART.VALID(4, 19, stringWithUnicodeChars))
  }

  property("string literal with unicode chars in language") {
    parseOne("\"\\u1234\"") shouldBe CONST_STRING(0, 8, PART.VALID(1, 7, "ሴ"))
  }

  property("should parse invalid unicode symbols") {
    parseOne("\"\\uqwer\"") shouldBe CONST_STRING(
      0,
      8,
      PART.INVALID(1, 7, "can't parse 'qwer' as HEX string in '\\uqwer'")
    )
  }

  property("should parse incomplete unicode symbol definition") {
    parseOne("\"\\u12 test\"") shouldBe CONST_STRING(0, 11, PART.INVALID(1, 10, "incomplete UTF-8 symbol definition: '\\u12'"))
    parseOne("\"\\u\"") shouldBe CONST_STRING(0, 4, PART.INVALID(1, 3, "incomplete UTF-8 symbol definition: '\\u'"))
  }

  property("string literal with special symbols") {
    parseOne("\"\\t\"") shouldBe CONST_STRING(0, 4, PART.VALID(1, 3, "\t"))
  }

  property("should parse invalid special symbols") {
    parseOne("\"\\ test\"") shouldBe CONST_STRING(0, 8, PART.INVALID(1, 7, "unknown escaped symbol: '\\ '. The valid are \b, \f, \n, \r, \t"))
  }

  property("should parse incomplete special symbols") {
    parseOne("\"foo \\\"") shouldBe CONST_STRING(0, 7, PART.INVALID(1, 6, "invalid escaped symbol: '\\'. The valid are \b, \f, \n, \r, \t"))
  }

  property("reserved keywords are invalid variable names") {
    List("if", "then", "else", "true", "false", "let").foreach { keyword =>
      val script =
        s"""let $keyword = 1
           |true""".stripMargin
      parseOne(script) shouldBe BLOCK(
        0,
        0,
        LET(0, 0, PART.INVALID(0, 0, "keywords are restricted"), CONST_LONG(0, 0, 1), Seq.empty),
        TRUE(0, 0)
      )
    }

    List("if", "then", "else", "let").foreach { keyword =>
      val script = s"$keyword + 1"
      parseOne(script) shouldBe BINARY_OP(0, 0, REF(0, 0, PART.INVALID(0, 0, "keywords are restricted")), BinaryOperation.SUM_OP, CONST_LONG(0, 0, 1))
    }
  }

  property("multisig sample") {
    val script =
      """
        |
        |let A = base58'PK1PK1PK1PK1PK1'
        |let B = base58'PK2PK2PK2PK2PK2'
        |let C = base58'PK3PK3PK3PK3PK3'
        |
        |let W = tx.bodyBytes
        |let P = tx.PROOF
        |let V = sigVerify(W,P,A)
        |
        |let AC = if(V) then 1 else 0
        |let BC = if(sigVerify(tx.bodyBytes,tx.PROOF,B)) then 1 else 0
        |let CC = if(sigVerify(tx.bodyBytes,tx.PROOF,C)) then 1 else 0
        |
        | AC + BC+ CC >= 2
        |
      """.stripMargin
    parseOne(script) // gets parsed, but later will fail on type check!
  }

  property("function call") {
    parseOne("FOO(1,2)".stripMargin) shouldBe FUNCTION_CALL(0, 8, PART.VALID(0, 3, "FOO"), List(CONST_LONG(4, 5, 1), CONST_LONG(6, 7, 2)))
    parseOne("FOO(X)".stripMargin) shouldBe FUNCTION_CALL(0, 6, PART.VALID(0, 3, "FOO"), List(REF(4, 5, PART.VALID(4, 5, "X"))))
  }

  property("function call on curly braces") {
    parseOne("{ 1 }(2, 3, 4)") shouldBe FUNCTION_CALL(
      0,
      14,
      PART.INVALID(0, 5, "'CONST_LONG(1)' is not a function name"),
      List(CONST_LONG(6, 7, 2), CONST_LONG(9, 10, 3), CONST_LONG(12, 13, 4))
    )
  }

  property("function call on round braces") {
    parseOne("(1)(2, 3, 4)") shouldBe FUNCTION_CALL(
      0,
      0,
      PART.INVALID(0, 0, "CONST_LONG(1) is not a function name"),
      List(2, 3, 4).map(CONST_LONG(0, 0, _))
    )
  }

  property("isDefined/extract") {
    parseOne("isDefined(X)") shouldBe FUNCTION_CALL(0, 0, PART.VALID(0, 0, "isDefined"), List(REF(0, 0, PART.VALID(0, 0, "X"))))
    parseOne("if(isDefined(X)) then extract(X) else Y") shouldBe IF(
      0,
      0,
      FUNCTION_CALL(0, 0, PART.VALID(0, 0, "isDefined"), List(REF(0, 0, PART.VALID(0, 0, "X")))),
      FUNCTION_CALL(0, 0, PART.VALID(0, 0, "extract"), List(REF(0, 0, PART.VALID(0, 0, "X")))),
      REF(0, 0, PART.VALID(0, 0, "Y"))
    )
  }

  property("getter") {
    isParsed("xxx   .yyy") shouldBe true
    isParsed("xxx.  yyy") shouldBe true

    parseOne("xxx.yyy") shouldBe GETTER(0, 0, REF(0, 0, PART.VALID(0, 0, "xxx")), PART.VALID(0, 0, "yyy"))
    parseOne(
      """
        |
        | xxx.yyy
        |
      """.stripMargin
    ) shouldBe GETTER(0, 0, REF(0, 0, PART.VALID(0, 0, "xxx")), PART.VALID(0, 0, "yyy"))

    parseOne("xxx(yyy).zzz") shouldBe GETTER(
      0,
      0,
      FUNCTION_CALL(0, 0, PART.VALID(0, 0, "xxx"), List(REF(0, 0, PART.VALID(0, 0, "yyy")))),
      PART.VALID(0, 0, "zzz")
    )
    parseOne(
      """
        |
        | xxx(yyy).zzz
        |
      """.stripMargin
    ) shouldBe GETTER(0, 0, FUNCTION_CALL(0, 0, PART.VALID(0, 0, "xxx"), List(REF(0, 0, PART.VALID(0, 0, "yyy")))), PART.VALID(0, 0, "zzz"))

    parseOne("(xxx(yyy)).zzz") shouldBe GETTER(
      0,
      0,
      FUNCTION_CALL(0, 0, PART.VALID(0, 0, "xxx"), List(REF(0, 0, PART.VALID(0, 0, "yyy")))),
      PART.VALID(0, 0, "zzz")
    )
    parseOne(
      """
        |
        | (xxx(yyy)).zzz
        |
      """.stripMargin
    ) shouldBe GETTER(0, 0, FUNCTION_CALL(0, 0, PART.VALID(0, 0, "xxx"), List(REF(0, 0, PART.VALID(0, 0, "yyy")))), PART.VALID(0, 0, "zzz"))

    parseOne("{xxx(yyy)}.zzz") shouldBe GETTER(
      0,
      0,
      FUNCTION_CALL(0, 0, PART.VALID(0, 0, "xxx"), List(REF(0, 0, PART.VALID(0, 0, "yyy")))),
      PART.VALID(0, 0, "zzz")
    )

    parseOne(
      """
        |
        | {
        |   xxx(yyy)
        | }.zzz
        |
      """.stripMargin
    ) shouldBe GETTER(0, 0, FUNCTION_CALL(0, 0, PART.VALID(0, 0, "xxx"), List(REF(0, 0, PART.VALID(0, 0, "yyy")))), PART.VALID(0, 0, "zzz"))

    parseOne(
      """
        |
        | {
        |   let yyy = aaa(bbb)
        |   xxx(yyy)
        | }.zzz
        |
      """.stripMargin
    ) shouldBe GETTER(
      0,
      0,
      BLOCK(
        0,
        0,
        LET(0, 0, PART.VALID(0, 0, "yyy"), FUNCTION_CALL(0, 0, PART.VALID(0, 0, "aaa"), List(REF(0, 0, PART.VALID(0, 0, "bbb")))), Seq.empty),
        FUNCTION_CALL(0, 0, PART.VALID(0, 0, "xxx"), List(REF(0, 0, PART.VALID(0, 0, "yyy"))))
      ),
      PART.VALID(0, 0, "zzz")
    )
  }

  property("crypto functions") {
    val hashFunctions = Vector("sha256", "blake2b256", "keccak256")
    val text          = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText   = ScorexBase58.encode(text.getBytes)

    for (f <- hashFunctions) {
      parseOne(s"$f(base58'$encodedText')".stripMargin) shouldBe
        FUNCTION_CALL(0, 0, PART.VALID(0, 0, f), List(CONST_BYTEVECTOR(0, 0, PART.VALID(0, 0, ByteVector(text.getBytes)))))
    }
  }

  property("show parse all input including INVALID") {
    val script =
      """let C = 1
        |foo
        |#@2
        |true""".stripMargin

    parseAll(script) shouldBe Seq(
      BLOCK(0, 0, LET(0, 0, PART.VALID(0, 0, "C"), CONST_LONG(0, 0, 1), Seq.empty), REF(0, 0, PART.VALID(0, 0, "foo"))),
      INVALID(0, 0, "#@", Some(CONST_LONG(0, 0, 2))),
      TRUE
    )
  }

  property("should parse INVALID expressions in the middle") {
    val script =
      """let C = 1
        |# /
        |true""".stripMargin
    parseOne(script) shouldBe BLOCK(
      0,
      0,
      LET(0, 0, PART.VALID(0, 0, "C"), CONST_LONG(0, 0, 1), Seq.empty),
      INVALID(0, 0, "#/", Some(TRUE(0, 0)))
    )
  }

  property("should parse INVALID expressions at start") {
    val script =
      """# /
        |let C = 1
        |true""".stripMargin
    parseOne(script) shouldBe INVALID(
      0,
      0,
      "#/",
      Some(
        BLOCK(
          0,
          0,
          LET(0, 0, PART.VALID(0, 0, "C"), CONST_LONG(0, 0, 1), Seq.empty),
          TRUE(0, 0)
        )
      )
    )
  }

  property("should parse INVALID expressions at end") {
    val script =
      """let C = 1
        |true
        |# /""".stripMargin
    parseAll(script) shouldBe Seq(
      BLOCK(0, 0, LET(0, 0, PART.VALID(0, 0, "C"), CONST_LONG(0, 0, 1), Seq.empty), TRUE(0, 0)),
      INVALID(0, 0, "#/")
    )
  }

  property("simple matching") {
    val code =
      """
        |
        | match tx {
        |    case a: TypeA => 0
        |    case b: TypeB => 1
        | }
        |
      """.stripMargin
    parseOne(code) shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(0, 0, Some(PART.VALID(0, 0, "a")), List(PART.VALID(0, 0, "TypeA")), CONST_LONG(0, 0, 0)),
        MATCH_CASE(0, 0, Some(PART.VALID(0, 0, "b")), List(PART.VALID(0, 0, "TypeB")), CONST_LONG(0, 0, 1))
      )
    )
  }

  property("multiple union type matching") {
    val code =
      """
        |
        | match tx {
        |    case txa: TypeA => 0
        |    case underscore : TypeB | TypeC => 1
        | }
        |
      """.stripMargin
    parseOne(code) shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(0, 0, Some(PART.VALID(0, 0, "txa")), List(PART.VALID(0, 0, "TypeA")), CONST_LONG(0, 0, 0)),
        MATCH_CASE(0, 0, Some(PART.VALID(0, 0, "underscore")), List(PART.VALID(0, 0, "TypeB"), PART.VALID(0, 0, "TypeC")), CONST_LONG(0, 0, 1))
      )
    )
  }

  property("matching expression") {
    val code =
      """
        |
        | match foo(x) + bar {
        |    case x:TypeA => 0
        |    case y:TypeB | TypeC => 1
        | }
        |
      """.stripMargin
    parseOne(code) shouldBe MATCH(
      0,
      0,
      BINARY_OP(
        0,
        0,
        FUNCTION_CALL(0, 0, PART.VALID(0, 0, "foo"), List(REF(0, 0, PART.VALID(0, 0, "x")))),
        BinaryOperation.SUM_OP,
        REF(0, 0, PART.VALID(0, 0, "bar"))
      ),
      List(
        MATCH_CASE(0, 0, Some(PART.VALID(0, 0, "x")), List(PART.VALID(0, 0, "TypeA")), CONST_LONG(0, 0, 0)),
        MATCH_CASE(0, 0, Some(PART.VALID(0, 0, "y")), List(PART.VALID(0, 0, "TypeB"), PART.VALID(0, 0, "TypeC")), CONST_LONG(0, 0, 1))
      )
    )
  }

  property("pattern matching with valid case, but no type is defined") {
    parseOne("match tx { case x => 1 } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          Some(PART.VALID(0, 0, "x")),
          List.empty,
          CONST_LONG(0, 0, 1)
        )
      )
    )
  }

  property("pattern matching with valid case, placeholder instead of variable name") {
    parseOne("match tx { case  _:TypeA => 1 } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          None,
          List(PART.VALID(0, 0, "TypeA")),
          CONST_LONG(0, 0, 1)
        )
      )
    )
  }

  property("pattern matching with no cases") {
    parseOne("match tx { } ") shouldBe INVALID(0, 0, "pattern matching requires case branches")
  }

  property("pattern matching with invalid case - no variable, type and expr are defined") {
    parseOne("match tx { case => } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          Some(PART.INVALID(0, 0, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          INVALID(0, 0, "expected expression")
        )
      )
    )
  }

  property("pattern matching with invalid case - no variable and type are defined") {
    parseOne("match tx { case => 1 } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          Some(PART.INVALID(0, 0, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          CONST_LONG(0, 0, 1)
        )
      )
    )
  }

  property("pattern matching with invalid case - no expr is defined") {
    parseOne("match tx { case TypeA => } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(0, 0, Some(PART.VALID(0, 0, "TypeA")), Seq.empty, INVALID(0, 0, "expected expression"))
      )
    )
  }

  property("pattern matching with invalid case - no var is defined") {
    parseOne("match tx { case :TypeA => 1 } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          Some(PART.INVALID(0, 0, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          Seq.empty,
          CONST_LONG(0, 0, 1)
        )
      )
    )
  }

  property("pattern matching with invalid case - expression in variable definition") {
    parseOne("match tx { case 1 + 1 => 1 } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          Some(PART.INVALID(0, 0, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          CONST_LONG(0, 0, 1)
        )
      )
    )
  }

  property("pattern matching with default case - no type is defined, one separator") {
    parseOne("match tx { case _: | => 1 } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          None,
          Seq(PART.INVALID(0, 0, "the type for variable should be specified: `case varName: Type => expr`")),
          CONST_LONG(0, 0, 1)
        )
      )
    )
  }

  property("pattern matching with default case - no type is defined, multiple separators") {
    parseOne("match tx { case  _: |||| => 1 } ") shouldBe MATCH(
      0,
      0,
      REF(0, 0, PART.VALID(0, 0, "tx")),
      List(
        MATCH_CASE(
          0,
          0,
          None,
          Seq(PART.INVALID(0, 0, "the type for variable should be specified: `case varName: Type => expr`")),
          CONST_LONG(0, 0, 1)
        )
      )
    )
  }
}
