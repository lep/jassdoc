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
