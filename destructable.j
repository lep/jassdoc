// Destructable Object API

// Facing arguments are specified in degrees

native CreateDestructable takes integer objectid, real x, real y, real face, real scale, integer variation returns destructable

/**
Creates a destructable at the coordinates ( x , y ).

@param objectid The rawcode of the destructable to be created.
@param x The x-coordinate of the destructable.
@param y The y-coordinate of the destructable.
@param face The facing of the destructable.
@param scale The X-Y-Z scaling value of the destructable.
@param variation The integer representing the variation of the destructable to be created.
*/
native CreateDestructableZ takes integer objectid, real x, real y, real z, real face, real scale, integer variation returns destructable

/**
Creates the dead version of a destructable at the coordinates ( x , y ).
If the destructable has no animations, it will show the destructable's default
form. If it has a death animation, but no decay animation, then the object will
be created in memory but will not visibly appear.

@param objectid The rawcode of the destructable to be created.
@param x The x-coordinate of the destructable.
@param y The y-coordinate of the destructable.
@param face The facing of the destructable.
@param scale The X-Y-Z scaling value of the destructable.
@param variation The integer representing the variation of the destructable to be created.
*/
native CreateDeadDestructable takes integer objectid, real x, real y, real face, real scale, integer variation returns destructable

/**
Creates the dead version of a destructable at the coordinates ( x , y , z ).
If the destructable has no animations, it will show the destructable's default
form. If it has a death animation, but no decay animation, then the object will
be created in memory but will not visibly appear.

@param objectid The rawcode of the destructable to be created.
@param x The x-coordinate of the destructable.
@param y The y-coordinate of the destructable.
@param z The z-coordinate of the destructable.
@param face The facing of the destructable.
@param scale The X-Y-Z scaling value of the destructable.
@param variation The integer representing the variation of the destructable to be created.
*/
native CreateDeadDestructableZ takes integer objectid, real x, real y, real z, real face, real scale, integer variation returns destructable

native RemoveDestructable takes destructable d returns nothing

native KillDestructable takes destructable d returns nothing

native SetDestructableInvulnerable takes destructable d, boolean flag returns nothing

native IsDestructableInvulnerable takes destructable d returns boolean

native EnumDestructablesInRect takes rect r, boolexpr filter, code actionFunc returns nothing

native GetDestructableTypeId takes destructable d returns integer

native GetDestructableX takes destructable d returns real

native GetDestructableY takes destructable d returns real

native SetDestructableLife takes destructable d, real life returns nothing

native GetDestructableLife takes destructable d returns real

native SetDestructableMaxLife takes destructable d, real max returns nothing

native GetDestructableMaxLife takes destructable d returns real

/**
Resurrects a destructable with the specified hit points.

@param d The destructable to resurrect. If it is not dead, there will be no effect.

@param life The amount of hit points the destructable will have when it is
resurrected. A value of 0, or any value above the destructable's maximum HP,
will give the destructable its maximum HP (as defined in the object editor).
Any value below 0.5 will give the destructable 0.5 hit points.

@param birth If true, the destructable will play its birth animation upon resurrection.

*/
native DestructableRestoreLife takes destructable d, real life, boolean birth returns nothing

native QueueDestructableAnimation takes destructable d, string whichAnimation returns nothing

native SetDestructableAnimation takes destructable d, string whichAnimation returns nothing

native SetDestructableAnimationSpeed takes destructable d, real speedFactor returns nothing

native ShowDestructable takes destructable d, boolean flag returns nothing

native GetDestructableOccluderHeight takes destructable d returns real

native SetDestructableOccluderHeight takes destructable d, real height returns nothing

/**
@async
*/
native GetDestructableName takes destructable d returns string

constant native GetTriggerDestructable takes nothing returns destructable
