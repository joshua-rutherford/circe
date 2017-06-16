package io.circe.numbers

import java.lang.StringBuilder
import java.math.BigInteger

/**
 * A parser for JSON numbers that produces values of type `J`.
 */
abstract class JsonNumberParser[J] {
  /**
   * Create a `J` value representing negative zero.
   */
  def createNegativeZero: J

  /**
   * Create a `J` value representing zero.
   */
  def createUnsignedZero: J

  /**
   * Create a `J` value from a [[scala.Long]].
   *
   * This method will be called if and only if the input is a valid non-zero [[scala.Long]] value.
   * The parser considers this to be the case even if the input contains a decimal point (followed
   * only by zeroes) or an exponential part.
   */
  def createLong(value: Long): J

  /**
   * Create a `J` value from a `BigInteger` representing the unscaled value and a [[scala.Long]]
   * representing the scale.
   *
   * This method will not ever be called if the input is zero or a valid [[scala.Long]]. It follows
   * the convention of `java.math.BigDecimal`, where the scale is the power of ten negated.
   * Additionally the scale will always be as small as possible (i.e. the unscaled value will not
   * end with any zeroes).
   */
  def createBigDecimal(unscaled: BigInteger, scale: Int): J

  /**
   * Create a `J` value from a `BigInteger` representing the unscaled value and a `BigInteger`
   * representing the scale.
   *
   * This method will not ever be called if the input is zero or a valid [[scala.Long]], or if the
   * scale fits in a [[scala.Int]]. It follows the convention of `java.math.BigDecimal`, where the
   * scale is the power of ten negated.
   */
  def createBiggerDecimal(unscaled: BigInteger, scale: BigInteger): J

  /**
   * Create a `J` value representing a number parsing failure (which will often be `null`).
   */
  def fail: J

  /**
   * Validate that a `CharSequence` is a valid JSON number.
   *
   * Adapted from the implementation in Jawn.
   */
  final def validate(input: CharSequence): Boolean = {
    val length = input.length
    var i = 0

    if (length == i) return false
    var c = input.charAt(i)

    if (c == '-') {
      i += 1
      if (length == i) return false
      c = input.charAt(i)
    }

    if (c == '0') {
      i += 1
      if (length == i) return true
      c = input.charAt(i)
    } else if ('1' <= c && c <= '9') {
      while ('0' <= c && c <= '9') {
        i += 1
        if (length == i) return true
        c = input.charAt(i)
      }
    } else return false

    if (c == '.') {
      i += 1
      if (length == i) return false
      c = input.charAt(i)

      if ('0' <= c && c <= '9') {
        while ('0' <= c && c <= '9') {
          i += 1
          if (length == i) return true
          c = input.charAt(i)
        }
      } else return false
    }

    if (c == 'e' || c == 'E') {
      i += 1
      if (length == i) return false
      c = input.charAt(i)

      if (c == '+' || c == '-') {
        i += 1
        if (length == i) return false
        c = input.charAt(i)
      }

      if ('0' <= c && c <= '9') {
        while ('0' <= c && c <= '9') {
          i += 1
          if (length == i) return true
          c = input.charAt(i)
        }
      } else return false
    }

    false
  }

  /**
   * Parse a `CharSequence` representing a JSON number.
   *
   * If the input is not a valid JSON number, the result is undefined (the method will never throw
   * an exception but will be nonsense).
   */
  final def parseUnsafe(input: CharSequence): J = {
    val length: Int = input.length
    var decIndex: Int = -1
    var expIndex: Int = -1
    var i: Int = 0

    while (i < length && expIndex == -1) {
      val c = input.charAt(i)
      if (c == '.') decIndex = i else if (c == 'e' || c == 'E') expIndex = i
      i += 1
    }

    parseUnsafeWithIndices(input, decIndex, expIndex)
  }

  /**
   * Parse a `CharSequence` representing a JSON number where the indices of a decimal point and
   * an `e` or `E` introducing an exponent (if any) are known.
   *
   * If the decimal point or `e` are not present in the input, the value of the appropriate index
   * must be `-1`.
   */
  final def parseUnsafeWithIndices(input: CharSequence, decIndex: Int, expIndex: Int): J = {
    val length: Int = input.length

    if (length == 0 || length <= decIndex || length <= expIndex) fail else {
      val sigNeg: Boolean = input.charAt(0) == '-'
      val sigDigStart: Int = if (sigNeg) 1 else 0
      var sigDigEnd: Int = if (expIndex >= 0) expIndex else length
      var expAdjust: Int = if (decIndex < 0) 0 else sigDigEnd - (decIndex + 1)

      while (sigDigEnd - 1 > sigDigStart && (input.charAt(sigDigEnd - 1) == '0' || sigDigEnd == decIndex + 1)) {
        sigDigEnd -= 1
        if (sigDigEnd != decIndex) expAdjust -= 1
      }

      val sigDigSkip = if (decIndex < sigDigEnd) decIndex else -1

      var expNeg: Boolean = false
      var expDigStart: Int = expIndex + 1

      if (expIndex >= 0) {
        val c = input.charAt(expIndex + 1)

        if (c == '-') {
          expNeg = true
          expDigStart = expDigStart + 1
        } else if (c == '+') {
          expDigStart = expDigStart + 1
        }

        while (expDigStart < length - 1 && input.charAt(expDigStart) == '0') {
          expDigStart += 1
        }
      }

      if (sigDigEnd - sigDigStart == 1 && input.charAt(sigDigStart) == '0') {
        if (sigNeg) createNegativeZero else createUnsignedZero
      } else {
        parse0(input, sigNeg, sigDigStart, sigDigEnd, sigDigSkip, expNeg, expDigStart, length, expAdjust)
      }
    }
  }

  private[this] final def parse0(
    input: CharSequence,
    sigNeg: Boolean,
    sigDigStart: Int,
    sigDigEnd: Int,
    sigDigSkip: Int,
    expNeg: Boolean,
    expDigStart: Int,
    expDigEnd: Int,
    expAdjust: Int
  ): J = {
    var sigLong: Long = 0L
    var sigBig: BigInteger = null
    var expLong: Long = 0L
    var expBig: BigInteger = null

    if (sigDigEnd - sigDigStart > 19) {
      sigBig = parseBigInteger(input, sigDigStart, sigDigEnd, sigDigSkip, sigNeg)
    } else {
      sigLong = parseLong(input, sigDigStart, sigDigEnd, sigDigSkip, sigNeg)

      if (sigLong == 0L) {
        sigBig = parseBigInteger(input, sigDigStart, sigDigEnd, sigDigSkip, sigNeg)
        if (sigBig == null) return fail
      }
    }

    if (expDigStart > 0 && (expDigEnd - expDigStart != 1 || input.charAt(expDigStart) != '0')) {
      if (expDigEnd - expDigStart > 19) {
        expBig = parseBigInteger(input, expDigStart, expDigEnd, -1, expNeg)
        if (expBig == null) return fail else {
          expBig = expBig.negate
        }
      } else {
        expLong = parseLong(input, expDigStart, expDigEnd, -1, !expNeg)

        if (expLong == 0L) {
          expBig = parseBigInteger(input, expDigStart, expDigEnd, -1, expNeg).negate
          if (expBig == null) return fail else {
            expBig = expBig.negate
          }
        }
      }
    }

    if (sigBig.eq(null) && expBig.eq(null) && expLong == 0L && expAdjust == 0) {
      createLong(sigLong)
    } else {
      parse1(sigLong, sigBig, expLong, expBig, expAdjust)
    }
  }

  private[this] final def parse1(
    sigLong: Long,
    sigBig: BigInteger,
    expLong: Long,
    expBig: BigInteger,
    expAdjust: Int
  ): J = {
    var expAdjustedInt: Int = 0
    var expAdjustedBig: BigInteger = expBig

    if (expBig.ne(null)) {
      expAdjustedBig = expBig.add(BigInteger.valueOf(expAdjust.toLong))
    } else if (expAdjust != 0 || expLong != 0L) {
      if (
        (expAdjust > 0 && expLong > (java.lang.Integer.MAX_VALUE - expAdjust).toLong) ||
        (expAdjust < 0 && expLong < (java.lang.Integer.MIN_VALUE - expAdjust).toLong) ||
        (expAdjust == 0 && expLong > java.lang.Integer.MAX_VALUE || expLong < java.lang.Integer.MIN_VALUE)
      ) {
        expAdjustedBig = BigInteger.valueOf(expLong).add(BigInteger.valueOf(expAdjust.toLong))
      } else {
        expAdjustedInt = expLong.toInt + expAdjust
      }
    }

    parse2(sigLong, sigBig, expAdjustedInt, expAdjustedBig)
  }

  private[this] final def parse2(
    sigLong: Long,
    sigBig: BigInteger,
    expInt: Int,
    expBig: BigInteger
  ): J = {
    if (expBig.ne(null)) {
      createBiggerDecimal(if (sigBig.ne(null)) sigBig else BigInteger.valueOf(sigLong), expBig)
    } else if (sigBig.ne(null)) {
      createBigDecimal(sigBig, expInt)
    } else if (expInt > 0) {
      createBigDecimal(if (sigBig.ne(null)) sigBig else BigInteger.valueOf(sigLong), expInt)
    } else if (expInt == 0) {
      createLong(sigLong)
    } else {
      var sigLongValue = sigLong
      var expIntValue = expInt

      if (sigLongValue > 0L) {
        while (expIntValue < 0 && sigLongValue <= -limitMult) {
          sigLongValue *= 10L
          expIntValue += 1
        }
      } else {
        while (expIntValue < 0 && sigLongValue >= limitMult) {
          sigLongValue *= 10L
          expIntValue += 1
        }
      }

      if (expIntValue < 0) {
        createBigDecimal(BigInteger.valueOf(sigLong), expInt)
      } else {
        createLong(sigLongValue)
      }
    }
  }

  private[this] final val posLimit = -java.lang.Long.MAX_VALUE
  private[this] final val negLimit = java.lang.Long.MIN_VALUE
  private[this] final val limitMult = posLimit / 10L

  private[this] final def parseLong(
    input: CharSequence,
    start: Int,
    end: Int,
    skip: Int,
    negate: Boolean
  ): Long = {
    val limit: Long = if (negate) negLimit else posLimit

    var result: Long = 0L
    var i: Int = start

    while (i < end) {
      val digit = input.charAt(i).toInt - 48

      if (i - start < 18) {
        result = result * 10L - digit
      } else {
        if (result < limitMult) {
          return 0L
        } else {
          result *= 10L

          if (digit > 0) {
            if (result < limit + digit) {
              return 0L
            } else {
              result -= digit
            }
          }
        }
      }

      i += 1
      if (i == skip) {
        i += 1
      }
    }

    if (negate) result else -result
  }

  private[this] final def parseBigInteger(
    input: CharSequence,
    start: Int,
    end: Int,
    skip: Int,
    negate: Boolean
  ): BigInteger = try {
    val substring = if (skip < 0) {
      input.subSequence(start, end).toString
    } else {
      val builder = new StringBuilder(input)
      builder.deleteCharAt(skip)
      builder.substring(start, end - 1)
    }

    val result = new BigInteger(substring)

    if (negate) result.negate else result
  } catch {
    case _: NumberFormatException => return null
  }
}
