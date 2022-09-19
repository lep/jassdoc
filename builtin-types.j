// make the parser happy
type code extends void

type handle extends void

type boolean extends void


/**
A 32-bit IEEE-754 float type.

<http://www.hiveworkshop.com/forums/lab-715/real-talk-floats-warcraft-3-a-270579/>

<http://www.hiveworkshop.com/forums/lab-715/100-accurate-pretty-fast-realtoindex-back-247170/>
*/
type real extends void

/**
A 32-bit twos-complement integer type.

Representations in Jass code:
* Decimal: Any number positive or negative, without the fractional part: -1, 0, 1337
* Octal: Any number beginning with 0: `07 == 7`, `08` is a syntax error.
* Hexadecimal: If the number starts with `0x` or `$` then the following comprises a hex value: `0x01da`, `$01da`
* Character: An ASCII charcater in single-quotes is interpreted as a byte value: `'d'` is 100.
* 4 characters (aka rawcode, FourCC): Four ASCII characters in single-quotes are interpreted as a 32-bit integer: 
`'dddd'` = (100 << 24) + (100 << 16) + (100 << 8 ) + 100 = 1677721600 + 6553600 + 25600 + 100 = 1684300900

@note Lua is also compiled with 32-bit integers (game's exe is 64-bit).
* Octal: The Jass2Lua transpiler does not support integers defined in octal representation and crashes. (Reforged, v1.32.10)
* Character: `'hfoo'` represents a human footman object code in Jass.
These are also called "raw codes" or FourCC.
In Jass, these are automatically read as integers by the Jass interpreter.
If you use Lua, you must manually convert a string of four characters to integer with `FourCC("hfoo")`.
This is true for ability IDs, unit/destructable types etc. in any place where the game's API needs an integer to refer to the type of an object.

<http://www.wc3c.net/showthread.php?t=99954>
*/
type integer extends void

/**
A length delimited byte addressable string type.

<http://www.hiveworkshop.com/forums/lab-715/documentation-string-type-240473/>
*/
type string extends void

