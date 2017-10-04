// String Utility API

/**
@pure
*/
native I2R  takes integer i returns real

/**
@pure
*/
native R2I  takes real r returns integer

/**
@pure
*/
native I2S  takes integer i returns string

/**
@pure
*/
native R2S  takes real r returns string

/**
Formats the real r into a string with supplied precision and width.
@param r The number to be converted
@param width The width of the string. If the width of the resulting conversion
             is too small the string will be filled with spaces.
             Use 0 for no padding.
@param precision The amount of decimal places.

@pure
*/
native R2SW takes real r, integer width, integer precision returns string

/**
Converts a string of digits to the represented number.
Returns `0` in case of an error.

@param s The string to be converted

@note This function only works for decimal strings. Hexadecimal or octal strings
are not supported.

@note If the input string starts with some valid input but ends in invalid input
this will return the conversion of the valid part: `S2I("123asd") == 123`.

@pure

*/
native S2I  takes string s returns integer



/**
Converts a string of digits to the represented number.
Returns `0` in case of an error.

@param s The string to be converted

@note This function only works for decimal strings. Hexadecimal or octal strings
are not supported.

@note If the input string starts with some valid input but ends in invalid input
this will return the conversion of the valid part: `S2R(".123asd") == 0.123`.

@pure
*/
native S2R  takes string s returns real

native GetHandleId takes handle h returns integer

/**
@pure

@note This function does bound-checking on the upper bound, e.g.
`SubString("test", 0, 9999) == "test"` but not on the lower bound:
````
SubString("", -3, 0) == null
SubString(null, -3, 0) == null
SubString("non-empty/not-null", -3, 0) != ""
SubString("non-empty/not-null", -3, 0) != null
````
<http://www.wc3c.net/showthread.php?p=1090749#post1090749>
*/
native SubString takes string source, integer start, integer end returns string

/**
Returns the length of the string in *bytes*.
@pure
*/
native StringLength takes string s returns integer

native StringCase takes string source, boolean upper returns string

/**
Case and slash insensitive hash function.
`StringHash("\\") == StringHash("/")`
`StringHash("AB") == StringHash("ab")`
@note See <http://www.hiveworkshop.com/forums/w-277/b-213272/> for the source-code of StringHash.
@pure
*/
native StringHash takes string s returns integer


/**
Reads a string out of some files and returns the result.
The result can differ between players with different languages.
Possible sources are the .fdf files and the war3map.wts file.
Returns source if no entry was found.

@bug Cannot assign it to a constant variable as it will crash the game.
`constant string foo = GetLocalizedString("bar")`

@async
*/
native GetLocalizedString takes string source returns string

/**
@async
*/
native GetLocalizedHotkey takes string source returns integer

