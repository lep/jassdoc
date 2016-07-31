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
@pure
*/
native S2I  takes string s returns integer

/**
@pure
*/
native S2R  takes string s returns real

native GetHandleId takes handle h returns integer

/**
@pure
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
*/
native GetLocalizedString takes string source returns string

native GetLocalizedHotkey takes string source returns integer

