// Terrain API

/**
Gets the cliff level at a point.

@param x x-coordinate (World) of the point.
@param y y-coordinate (World) of the point.

@note Walkable destructables add their effective cliff level to the terrain across their pathing maps, i.e., if the terrain at some point
without destructables has a cliff height of a and the destructable covering that point has an effective cliff height of b, this function returns a + b. If there
are multiple walkable destructables intersecting at the requested point, the function returns a + max(b1, b2, ...). If the declared cliff height of a destructable
is negative, it will have an effective cliff height of 0.
*/
native GetTerrainCliffLevel         takes real x, real y returns integer

/**
Sets the tint of the water.

@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha 0-255 opaqueness (value mod 256).

@note The default is 255 for all parameters.

@bug In HD, the alpha setting seems to only affect the water surface.
*/
native SetWaterBaseColor            takes integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets whether terrain deformations also affect the water mesh on top of it.

@param val If this is true, terrain deformations will affect the water mesh.

@note This is only during transitions of terrain deformations, i.e., for temporary terrain deformations and during the transition
of permanent terrain deformations. After a permanent terrain deformation has completed, the water deformation will revert instantly.

@note The default is false.
*/
native SetWaterDeforms              takes boolean val returns nothing

native GetTerrainType               takes real x, real y returns integer

native GetTerrainVariance           takes real x, real y returns integer

native SetTerrainType               takes real x, real y, integer terrainType, integer variation, integer area, integer shape returns nothing

/**
Returns if a specific pathingtype is set at the location.
@note Returns true if the pathingtype is *not* set, false if it *is* set.
*/
native IsTerrainPathable            takes real x, real y, pathingtype t returns boolean

native SetTerrainPathable           takes real x, real y, pathingtype t, boolean flag returns nothing
