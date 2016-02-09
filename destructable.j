// Destructable Object API

// Facing arguments are specified in degrees

native CreateDestructable takes integer objectid, real x, real y, real face, real scale, integer variation returns destructable

native CreateDestructableZ takes integer objectid, real x, real y, real z, real face, real scale, integer variation returns destructable

native CreateDeadDestructable takes integer objectid, real x, real y, real face, real scale, integer variation returns destructable

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

native DestructableRestoreLife takes destructable d, real life, boolean birth returns nothing

native QueueDestructableAnimation takes destructable d, string whichAnimation returns nothing

native SetDestructableAnimation takes destructable d, string whichAnimation returns nothing

native SetDestructableAnimationSpeed takes destructable d, real speedFactor returns nothing

native ShowDestructable takes destructable d, boolean flag returns nothing

native GetDestructableOccluderHeight takes destructable d returns real

native SetDestructableOccluderHeight takes destructable d, real height returns nothing

native GetDestructableName takes destructable d returns string

constant native GetTriggerDestructable takes nothing returns destructable
