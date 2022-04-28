/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */
/*
//1
#include "bitXor.c"
#include "tmin.c"

//2
#include "isTmax.c"
#include "allOddBits.c"
#include "negate.c"

//3
#include "isAsciiDigit.c"
#include "conditional.c"
#include "isLessOrEqual.c"

//4
#include "logicalNeg.c"
#include "howManyBits.c"

//float
#include "floatScale2.c"
#include "floatFloat2Int.c"
#include "floatPower2.c~ (1 << 31);"

//
#include "bitCount.c"
*/
/* 
 * floatAbsVal - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatAbsVal(unsigned uf) {
  unsigned zero_on_top, exp, frac, abs;

  zero_on_top = ~ (1 << 31); // 0111...111
  exp = 0xFF << 23; // exp field
  frac = 0x7FFFFF; // frac field
  abs = uf & zero_on_top;

  if ((uf & exp) ^ exp) // exp != 11111111
	return abs;
  if (uf & frac) // frac != 00000000
	return uf;
  return abs;
}
/* 
 * floatNegate - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatNegate(unsigned uf) {
  unsigned one_on_top, exp, frac, neg;

  one_on_top = 1 << 31; // 1000...000
  exp = 0xFF << 23; // exp field
  frac = 0x7FFFFF; // frac field
  neg = uf ^ one_on_top;

  if ((uf & exp) ^ exp) // exp != 11111111
	return neg;
  if (uf & frac) // frac != 00000000
	return uf;
  return neg;
}
/* 
 * floatPower2 - Return bit-level equivalent of the expression 2.0^x
 *   (2.0 raised to the power x) for any 32-bit integer x.
 *
 *   The unsigned value that is returned should have the identical bit
 *   representation as the single-precision floating-point number 2.0^x.
 *   If the result is too small to be represented as a denorm, return
 *   0. If too large, return +INF.
 * 
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. Also if, while 
 *   Max ops: 30 
 *   Rating: 4
 */
unsigned floatPower2(int x) {
  if (x > 128) return 0xFF << 23; // too large
  if (x < -149) return 0; // too small

  if (x < -126) return 1 << (-x - 126 - 1); // denormalized

  return (x + 127) << 23; // normalized
}
//#include "floatScale2.c"
//#include "isLess.c"
/*
 * isPower2 - returns 1 if x is a power of 2, and 0 otherwise
 *   Examples: isPower2(5) = 0, isPower2(8) = 1, isPower2(0) = 0
 *   Note that no negative number is a power of 2.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int isPower2(int x) {
  int is_positive, only_one, is_not_zero;

  is_positive = x >> 31; // 000...000 if pos, 111...111 if neg
  is_positive = ~ is_positive; // 111...111 if pos, 000...000 if neg

  only_one = (~ 0) + x; // 000...0011...111 if only one 1
  only_one = only_one & x; // 000...000 if only one 1 + if x=0
  only_one = ! only_one; // 1 if only one 1 + if x=0

  is_not_zero = !! x;

  return is_positive & only_one & is_not_zero;
}
/* 
 * logicalNeg - implement the ! operator, using all of 
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int logicalNeg(int x) {
  int result;
  
  result = x;

  result = result | (result >> 16); // ???...???000...000 if no 1
  result = result | (result >> 8); // ???...???00000000 if no 1
  result = result | (result >> 4); // ???...???0000 if no 1
  result = result | (result >> 2); // ???...???00 if no 1
  result = result | (result >> 1); // ???...???0 if no 1
  result = ~ result; // ???...???1 if no 1

  return result & 1;
}
/* 
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3 
 */
int logicalShift(int x, int n) {
  int one_on_top, filter, zero_or_not;

  one_on_top = 1 << 31;
  filter = (one_on_top >> n) << 1; // 1111000...000 if n=4
  filter = ~ filter; // 0000111...111 if n=4

  zero_or_not = (!!n << 31) >> 31; // 000...000 if zero, 111...111 if nonzero
  zero_or_not = ~ zero_or_not; // 111...111 if zero, 000...000 if nonzero
  filter = filter | zero_or_not; // 111...111 if n=0, 0000111...111 if n=4

  return (x >> n) & filter;
}
/* 
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c) {
  int shift, filter, byte_block;

  shift = n << 3; // n * 8

  filter = 0xFF << shift; // 0000FF00 if n=1
  filter = ~ filter; // FFFF00FF if n=1
  byte_block = c << shift; // 0000??00 if n=1

  return (x & filter) + byte_block;
}
/* 
 * rotateRight - Rotate x to the right by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321,4) = 0x187654321
 *   Legal ops: ~ & ^ | + << >> !
 *   Max ops: 25
 *   Rating: 3 
 */
int rotateRight(int x, int n) {
  int zero_or_not, all_one, one_on_top, minus_n, n_minus_one;
  int shift_filter, shift_block, rotate_filter, rotate_block;

  zero_or_not = ((!!n) << 31) >> 31; // 000...000 if zero, 111...111 if nonzero
  all_one = ~ 0; // 111...111
  minus_n = (~ n) + 1;
  n_minus_one = n + ~0;

  rotate_filter = all_one << n; // 111...1110000 if n=4
  rotate_filter = ~ rotate_filter; // 000...0001111 if n=4
  rotate_block = x & rotate_filter; // 000...000???? if n=4
  rotate_block = zero_or_not & rotate_block; // 000...000 if n=0
  rotate_block = rotate_block << (32 + minus_n); // ????000...000 if n=4
  
  one_on_top = zero_or_not << 31; // 000...000 if n=0
  shift_filter = one_on_top >> n_minus_one; // 1111000...000 if n=4
  shift_filter = ~ shift_filter; // 0000111...111 if n=4
  shift_block = x >> n; // 1111???...??? or 0000???...??? if n=4
  shift_block = shift_block & shift_filter; // 0000???...??? if n=4
  
  return rotate_block + shift_block;
}
/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x80000001) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
  int msb, nsb, sat_block, normal_block, flow;

  msb = !! (x & (1 << 31)); // most significant bit, 0 if of, 1 if uf
  msb = (msb << 31) >> 31; // 000...000 if of, 111...111 if uf
  nsb = !! (x & (1 << 30)); // next significant bit, 1 if of, 0 if uf
  nsb = (nsb << 31) >> 31; // 111...111 if of, 000...000 if uf

  sat_block = nsb + (1 << 31); // 011...111 if of, 100...000 if uf
  normal_block = x << 1;
  flow = msb ^ nsb; // 111...111 if of/uf, 000...000 otherwise
  sat_block = sat_block & flow; // 000...000 if normal
  normal_block = normal_block & (~ flow); // 000...000 if of/uf

  return sat_block + normal_block;
}
/* 
 * sign - return 1 if positive, 0 if zero, and -1 if negative
 *  Examples: sign(130) = 1
 *            sign(-23) = -1
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 10
 *  Rating: 2
 */
int sign(int x) {
  int fullof, ret, zero_or_not;

  fullof = x >> 31; // 000...000 if pos, 111...111 if neg
  ret = (fullof << 1) + 1; // 000...001 if pos, 111...111 if neg
  zero_or_not = (!!x << 31) >> 31; // 000...000 if zero, 111...111 if nonzero

  return zero_or_not & ret;
}
//#include "subtractionOK.c"
/* 
 * thirdBits - return word with every third bit (starting from the LSB) set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int thirdBits(void) {
  int six, twelve, twenty_four, thirty;

  six = 36; // 100100
  twelve = (six << 6) + six; // 100100100100
  twenty_four = (twelve << 12) + twelve; // 100...100
  thirty = (twenty_four << 6) + six;  // 100...100

  return (thirty << 1) + 1; // 100...1001
}
/* 
 * upperBits - pads n upper bits with 1's
 *  You may assume 0 <= n <= 32
 *  Example: upperBits(4) = 0xF0000000
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 10
 *  Rating: 1
 */
int upperBits(int n) {
  int one_on_top, n_minus_one;

  one_on_top = !!n << 31; // 000...000 if n=0
  n_minus_one = n + ~0;

  return one_on_top >> n_minus_one; 
}
