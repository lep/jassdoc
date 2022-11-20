// MathAPI

/**
Converts degrees into radians. This is similar to multiplying the degree value by pi / 2.

@note This is slightly more accurate than multiplying the degree value
by `bj_PI / 2`. `bj_PI` has a value of 3.14159. This native uses a pi value closer to 3.141592496.

@param degrees The degree input.

@pure
*/
native Deg2Rad takes real degrees returns real

/**
Converts a radian value into its degree equivalent.

@param radians The radian value to be converted.

@pure
*/
native Rad2Deg takes real radians returns real


/**
Takes a real value input in radians and returns its sine value. The domain of
the input is all real numbers and the range of the output is -1 to 1 inclusive.

@param radians The input radians.

@pure
*/
native Sin takes real radians returns real

/**
Takes a real value input in radians and returns its cosine value. The domain of
the input is all real numbers and the range of the output is -1 to 1 inclusive.

@param radians The input radians.

@pure
*/
native Cos takes real radians returns real

/**
Takes a real value input in radians and returns its tangent value.

@param radians The input radians.
@pure
*/
native Tan takes real radians returns real



/**
Arcsine, one of inverse trigonometric functions. The result is returned in
radians in range [-Pi/2;Pi/2].
Returns 0 for invalid input.

@param y A value between -1 and 1.
@pure
*/
native Asin takes real y returns real

/**
Arccos, one of inverse trigonometric functions. The result is returned in
radians in range [-Pi/2;Pi/2].
Returns 0 for invalid input.

@param x A value between -1 and 1.
@pure
*/
native Acos takes real x returns real

/**
Arctangen, one of the inverse trigonometric functions. The result is returned
in radians in range [-π/2, π/2].
Returns 0 for invalid input.

@param x A value between -1 and 1.
@pure
*/
native Atan takes real x returns real

/**
Arctangent function with two arguments.
The result is returned in radians in range (-Pi;Pi].
Returns 0 if x and y are both 0
@pure
*/
native Atan2 takes real y, real x returns real



/**
Returns the square root of x.
If x is less than or equal to zero this returns 0.0

@param x Should be greater than or equal to 0.

@pure
*/
native SquareRoot takes real x returns real



/**
Computes x to the y'th power.
If y is zero this returns 1.0 and if both x is zero and y is less than zero this returns 0.0

@pure
*/
native Pow takes real x, real power returns real

// Bit Operations

/**
Returns the result of connecting all bits of both numbers using OR (in regards of binary numeral system). It returns a number with bits, being set in at least one of the numbers.

3v1 => 3 (0011 v 0001 => 0011)
2v5 => 7 (0010 v 0101 => 0111)


@pure
@patch 1.31
*/
native BlzBitOr                                    takes integer x, integer y returns integer

/**
Returns the result of connecting all bits of both numbers using AND (in regards of binary numeral system). It tells which bits are set for both integers.

3&1 => 1 (0011 & 0001 => 0001)
2&1 => 0 (0010 & 0001 => 0000)
11&7 => 3 (1011 & 0111 => 0011)
13&5 => 5 (1101 & 0101 => 0101)
12&6 => 4 (1100 & 0100 => 0100)


@pure
@patch 1.31
*/
native BlzBitAnd                                   takes integer x, integer y returns integer

/**
Returns the result of connecting all bits of both numbers using XOR (Difference) (in regards of binary numeral system). Each Bit being different between x and y becomes 1; every bit being equal becomes 0.

2 xor 5 => 7 (0010 xor 0101 => 0111)
6 xor 8 => 14  (0110 xor 1000 => 1110)


@pure
@patch 1.31
*/
native BlzBitXor                                   takes integer x, integer y returns integer 

/**
@pure
@patch 1.31
*/
native BlzConvertColor                             takes integer a, integer r, integer g, integer b returns integer

/**
@patch 1.32
@pure
*/
constant native MathRound takes real r returns integer
