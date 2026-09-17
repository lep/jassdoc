// This files contains an unsorted, possibly duplicate list of native functions
// that have been removed or incompatibly changed (e.g. during beta) and thus
// cannot be listed in any other file.

/**
Exact function signature unknown.

Maybe it was never supposed to be a native function?
A function of the same name is always generated as a part of preload files to run all the
preload lines after loading a preload file.

It existed at least until 1.32.10.x in Lua and removed in v1.33.0.18897 PTR.

@note See: `Preload`
*/
native PreloadFiles takes nothing returns nothing

/**
Removed from common.j after Reforged v1.36.1.20719-w3, missing in v2.0.2.22692-w3t.

May have been a NOOP internally for longer. See equivalent usage examples in `/extra-jass/reforged-dzapi.j`
*/
native RequestExtraIntegerData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns integer

/**
Removed from common.j after Reforged v1.36.1.20719-w3, missing in v2.0.2.22692-w3t.

May have been a NOOP internally for longer. See equivalent usage examples in `/extra-jass/reforged-dzapi.j`
*/
native RequestExtraBooleanData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns boolean

/**
Removed from common.j after Reforged v1.36.1.20719-w3, missing in v2.0.2.22692-w3t.

May have been a NOOP internally for longer. See equivalent usage examples in `/extra-jass/reforged-dzapi.j`
*/
native RequestExtraStringData                      takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns string

/**
Removed from common.j after Reforged v1.36.1.20719-w3, missing in v2.0.2.22692-w3t.

May have been a NOOP internally for longer. See equivalent usage examples in `/extra-jass/reforged-dzapi.j`
*/
native RequestExtraRealData                        takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns real
