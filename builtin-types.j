// make the parser happy
type code extends void

type handle extends void

/**
Booleans might be `true` or `false`, "common.j" variables `TRUE` and `FALSE` were added to allow you to use them, but it is not a good idea to do so. Using `true` is faster than getting a value of variable `TRUE`.

Comparision expresions `==`, `<=`, `<`, `>`, `>=`, `!=` return `boolean`.

Statements `if`, `elseif` and `exitwhen` take `boolean` as argument.
*/
type boolean extends void


/**
A 32-bit IEEE-754 float type.

@note The equality `==` and inequality `!=` operators are different for type `real.`

`0.0 == 0.0009` is true, effectively the precision is set to `0.001` (epsilon)
in the Jass VM. As a result, `0.0 == 0.001` is false.

However, the `!=` operator does not have its accuracy limited, so
`0.0 != 0.0009` will return true too. The inequality operator can be used to
compare reals of arbitrary precision (as long they are actually different in
binary).

@note In-depth analysis of floating-point behavior of `real` in Jass:
[real Talk - Floats in Warcraft 3](https://www.hiveworkshop.com/threads/real-talk-floats-in-warcraft-3.270579/).

@note (2014) Typecast exploit conversion between real and integer:
[100% accurate and pretty fast realToIndex and back](https://www.hiveworkshop.com/threads/100-accurate-and-pretty-fast-realtoindex-and-back.247170/).
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

* Octal: The Jass2Lua compiler does not support integers defined in octal representation and crashes. (Reforged, v1.32.10)
* Charater: same as Jass
* 4 characters: `'hfoo'` represents a human footman object code in Jass.
These are also called "raw codes" or FourCC.
In Jass, these are automatically read as integers by the Jass interpreter.
If you use Lua, you must manually convert a string of four characters to integer with `FourCC("hfoo")`.
This is true for ability IDs, unit/destructable types etc. in any place where the game's API needs an integer to refer to the type of an object.

<http://www.wc3c.net/showthread.php?t=99954>
*/
type integer extends void

/**
A length delimited byte addressable string type.

JASS strings are written inside double-quote `""` (example: `"example"`).
There are 2 null values for strings: `""` and `null`.

**Localized Strings**

Some game API functions (natives) support the usage of `TRIGSTR_` prefix at the
start of the string. Such strings are read from the "war3map.wts" file to allow
their localization. For example: "TRIGSTR_001" will be replaced by the game with
the string number 1 defined in the .wts file.

**Control Characters**

Both `\n` and `\r` are treated as new-line characters (v1.32.10).

`\"` Allows to include double-quotes inside strings, example:
`"Then he said \"Hello!\" and that was it."`

`\\` Allows to use the backslash `\` character, because a single `\` is used to escape the next character. When you use `\\` the game considers it a single `\`,
if you only place a single `\` it may cause a compiler crash.

- Correct: `"Here comes the backslash: \\ end"`
- Correct: `"Here comes the new line: \n end"`
- Incorrect: `"The space character should not be escaped: \ end"`

**Formatting**

- `|cAARRGGBB` change the color of the following text in the string.
`AA` is alpha, `RR` is red, `GG` is green and `BB` is blue.
The colors are specified as a hexadecimal values example: `|cffcc1100`.

- `|r` Will reset the color of the text back to default (white).

- `|n` Equivalent to new-line in some places, but might not work correctly in
other places. For example, `DisplayTimedTextToPlayer` works with `\n`
but not with `|n` (v1.32.10).

<http://www.hiveworkshop.com/forums/lab-715/documentation-string-type-240473/>

@note **Limitations**

The maximum string length in JASS code is limited to ca. 4096 ASCII characters.
WorldEditor and some game functions/places may have lower limits (esp. GUI triggers).

v1.32.10: Quest description is limited to 30000 ASCII in WorldEditor, but only
the last 2456 characters are displayed in-game. A building's extended tooltip is
limited to 8191 ASCII characters in-game and in WorldEditor.
*/
type string extends void

