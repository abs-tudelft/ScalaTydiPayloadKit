package TydiPackaging

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TydiBinaryTest extends AnyFunSuite with Matchers {

  test("binary glue - basic concatenation and string representation") {
    // Test 1: 16-bit binary
    val bin1 = TydiBinary(BigInt("1111000010101010", 2), 16)
    val string1 = bin1.toString
    assert(string1 == "0b1111000010101010")

    // Test 2: 12-bit binary (partial byte)
    val bin2 = TydiBinary(BigInt("111110101010", 2), 12)
    val string2 = bin2.toString
    assert(string2 == "0b111110101010")

    // Test 3: Concatenation test
    val lastBin = TydiBinary(BigInt("101", 2), 3) // Value = 5
    val charBin = TydiBinary(BigInt("01000011", 2), 8) // Value = 67 or 0x43
    val package_ = lastBin.concat(charBin)
    
    val packageString = package_.toString
    assert(packageString == "0b01000011101")

    // Test 4: More complex concatenation
    val bin3 = TydiBinary(BigInt("110010101011", 2), 12)
    val string3 = bin3.toString
    assert(string3 == "0b110010101011")

    val bin4 = TydiBinary(BigInt("0000111111011110", 2), 16)
    val string4 = bin4.toString
    assert(string4 == "0b0000111111011110")

    val result2 = bin3.concat(bin4)
    val resultString2 = result2.toString
    assert(resultString2 == "0b0000111111011110110010101011")

    // Test 5: Split functionality
    val (recovered3, recovered4) = result2.splitLow(12)
    println(s"recovered3: $recovered3 (recovered4: $recovered4)")
    assert(recovered3.toString == "0b110010101011")
    assert(recovered4.toString == "0b0000111111011110")
  }

  test("binary from hex values") {
    // Test creating binary from hex values
    val bin1 = TydiBinary(BigInt("AB0C", 16), 16)
    assert(bin1.toString == "0b1010101100001100")

    val bin2 = TydiBinary(BigInt("0F", 16), 8)
    assert(bin2.toString == "0b00001111")
  }

  test("binary concatenation with split") {
    val bin1 = TydiBinary(BigInt("10101010", 2), 8)
    val bin2 = TydiBinary(BigInt("11110000", 2), 8)
    
    val concatenated = bin1.concat(bin2)
    assert(concatenated.length == 16)
    assert(concatenated.toString == "0b1111000010101010")

    val (left, right) = concatenated.splitLow(8)
    assert(left.toString == "0b10101010")
    assert(right.toString == "0b11110000")
  }

  test("binary from integers") {
    // Test converting from integer values
    val value1: Long = 123456789L
    val binary1 = TydiBinary(BigInt(value1), 64)
    assert(binary1.length == 64)
    println(s"number: $value1, tydi: $binary1")

    val value2: Long = 12345678L
    val binary2 = TydiBinary(BigInt(value2), 64)
    assert(binary2.length == 64)
  }

  test("binary from floating point") {
    val value: Double = 3.14159
    val longBits = java.lang.Double.doubleToRawLongBits(value)
    val binary = TydiBinary(BigInt(longBits), 64)
    
    assert(binary.length == 64)
  }

  test("binary from character") {
    val value: Char = 'm'
    val binary = TydiBinary(BigInt(value.toInt), 8)
    
    assert(binary.length == 8)
    assert(binary.data == BigInt(value.toInt))
  }

  test("empty binary") {
    val empty = TydiBinary.empty
    assert(empty.length == 0)
    assert(empty.data == BigInt(0))
  }

  test("binary padding in string representation") {
    // Test that binString properly pads with zeros
    val bin1 = TydiBinary(BigInt("1", 2), 8)
    assert(bin1.toString == "0b00000001")

    val bin2 = TydiBinary(BigInt("101", 2), 10)
    assert(bin2.toString == "0b0000000101")
  }

  test("complex split scenarios") {
    // Split at different positions
    val bin = TydiBinary(BigInt("111100001010", 2), 12)
    
    val (left1, right1) = bin.splitLow(4)
    assert(left1.toString == "0b1010")
    assert(right1.toString == "0b11110000")

    val (left2, right2) = bin.splitLow(8)
    assert(left2.toString == "0b00001010")
    assert(right2.toString == "0b1111")
  }

  test("large binary values") {
    val largeBin = TydiBinary(BigInt("1" * 128, 2), 128)
    assert(largeBin.length == 128)
    assert(largeBin.toString == "0b" + "1" * 128)
  }
}
