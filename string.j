// String Utility API

/**
Returns a real representation for integer i.

Lua: If i is not an integer or i is null, raises an error.

@pure
*/
native I2R  takes integer i returns real

/**
Returns an integer representation for real r. The output will be rounded towards 0 if it is a real number.

Lua: Only raises an error if r is null.

For extermely large values the minimum/maximum representable signed integer will be returned
(e.g. for Lua: `math.mininteger` and `math.maxinteger`)

@note NaN is not a possible value in Warcraft 3 (always reset to 1.0)

@pure
*/
native R2I  takes real r returns integer

/**
Returns the string representation for integer i.

Lua: Raises an error if i is null or has no integer representation.

@pure
*/
native I2S  takes integer i returns string

/**
Returns a string representation for real r with precision of 3 digits.
The real is correctly rounded to nearest to fit within the precision.

Lua: Raises an error if r is null.

**Example:**

`R2S(1.12) --> 1.120`
Equivalent to: `R2SW(r, 0, 3)` and Lua: `string.format("%.3f", r)`

@note See: `R2SW`

@pure
*/
native R2S  takes real r returns string

/**
Returns a string representation for real r with precision digits and width.
The real is correctly rounded to nearest to fit within the precision.

Lua: Raises an error if r is null.

Works similar to C/C++ [printf](https://www.cplusplus.com/reference/cstdio/printf/),
but does not support negative width (left-align with right padding).

**Example (Lua):**

	R2SW(31.1235, 5, 3) --> 31.124
	R2SW(1, 5, 0) --> two spaces followed by number
	  1.0

@param r The number to be converted
@param width The width of the string. If the width of the resulting conversion
             is too small the string will be filled with spaces.
             Use 0 for no padding.
@param precision The amount of decimal places. The minimum possible precision is 1 (automatically set).

@note See: `R2S` for a simple converter with preset values

@pure
*/
native R2SW takes real r, integer width, integer precision returns string

/**
Returns an integer by parsing the string for a number.

For values too big or too small, returns max/min integer respectively.
For an empty string or text that doesn't start with a number, returns 0.


Lua: For null raises an error.

**Examples (Lua):**

	S2I("") -- 0
	S2I("-123") -- -123
	S2I("-99999999") -- -2147483648
	S2I("99999999") -- 2147483647
	S2I("123abc") -- 123
	S2I("abc123") -- 0
	S2I(nil) -- error

@param s The string to be converted

@note This function only works for decimal strings. Hexadecimal or octal strings
are not supported.

@note The parser stops at the first non-number character [0-9.].
If the input string starts with some valid input but ends in invalid input
this will return the conversion of the valid part: `S2I("123asd") == 123`.

@pure

*/
native S2I  takes string s returns integer



/**
Returns a real by parsing the string for a number.
Returns 0 for: values too big or too small, an empty string or text that doesn't start with a number.

Lua: For null raises an error.

@param s The string to be converted

@note This function only works for decimal strings. Hexadecimal or octal strings
are not supported.

@note The parser stops at the first non-number character [0-9.] - does not support comma `,` as a decimal point.
If the input string starts with some valid input but ends in invalid input
this will return the conversion of the valid part: `S2R(".123asd") == 0.123`.

@pure
*/
native S2R  takes string s returns real

/**
returns the internal index of the given handle.

**Example:** `GetHandleId(Player(0)) -> 1048584`

@param h Handle

@note Sometimes the handle ID may be different between clients.

@patch 1.24b
*/
native GetHandleId takes handle h returns integer

/**
Returns a new substring from the interval [start, end) - inclusive, exclusive.
Positions are zero-indexed.
For empty or invalid out-of-bounds values returns an empty string "" (in Lua).

For start>end returns substring beginning with start until the actual end of string.
For start<0 returns an empty string.

**Examples (Lua):**

    SubString("abc", 0, 0) == ""
    SubString("abc", 0, 1) == "a"
    SubString("abc", 2, 3) == "c"
    SubString("abc", 0, 3) == "abc"
    SubString("abcdef", 2, 0) == "cdef"

@param source Text string
@param start Starting position, zero-indexed, inclusive.
@param end Last position, zero-indexed, exclusive.

@pure
*/
native SubString takes string source, integer start, integer end returns string

/**
Returns the length of the string in *bytes*.
This means Unicode (non-ASCII) characters will take up and return a higher byte count than there are letters.

**Example**: `StringLength("я")` returns 2.

@pure
*/
native StringLength takes string s returns integer

/**
Turns the text to upper/lower case and returns it. Only works for ASCII characters (A-Z), not Unicode (Дружба).

@param source Text string

@param upper True: turn to UPPER CASE. False: turn to lower case.

@pure
*/
native StringCase takes string source, boolean upper returns string

/**
Returns a string hash for the given string. The string is normalized before hashing.

The hash is supposed to be case-insensitive of the input string:
this works for ASCII and (Reforged) some small subset of Unicode (Latin Supplement, Cyrillic...).
Also the backslash is the same as forward slash: `/` and `\`.
A probable explanation for this is the usage of file paths, since the game runs on Windows and Mac OS/OSX.
StringHash is also used for variable lookup: string name -> integer index.

`StringHash("\\") == StringHash("/")`
`StringHash("AB") == StringHash("ab")`

@note Code for the algorithm ["SStrHash2"](https://www.hiveworkshop.com/threads/bits-of-interest.213272/) via ["1997 Dr Dobbs article"](http://burtleburtle.net/bob/hash/doobs.html).

@note *Breaking:* The hashing of multi-byte characters (Unicode) was changed in v1.30.0/1.31.1.
It's unknown if hashes of these characters are different in old versions between Windows/Mac OS
or depends on OS-default character page settings (non-Unicode programs on Windows).

@pure

@patch 1.24a
*/
native StringHash takes string s returns integer


/**
returns a translated string for the client's local language.
Without an available translation, returns `source`.

The result will differ between players with different languages.
Possible sources are the .fdf files and the war3map.wts file.

**Example:** `GetLocalizedString("REFORGED")` -> "Reforged"

@bug (Jass) Cannot assign it to a constant variable as it will crash the game.
`constant string foo = GetLocalizedString("bar")`

@async
*/
native GetLocalizedString takes string source returns string

/**
Returns the `integer` hotkey for a specific game action à la `"GAMEOVER_QUIT_GAME"`.
You can look up potential values in `UI\FrameDef\GlobalStrings.fdf`.

@note To define own values you have to import a file named `war3mapMisc.txt`
into your map. A sample `war3mapMisc.txt` could look like this:

    [Hotkeys]
    ,=44
    !='!'
    A='A'
    B='B'
    C='C'
    // etc.

See also <https://www.hiveworkshop.com/threads/chrord.274579/>.

@async
*/
native GetLocalizedHotkey takes string source returns integer

/**
@patch 1.32
*/
constant native ParseTags               takes string taggedString returns string

