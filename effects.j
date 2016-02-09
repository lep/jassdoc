// Effects API

/**
Adds the weather effect with id effectID to the rect where.
@note To understand more about weather effects nature, I advise to read
Ammorth's article about weather effects: <http://www.wc3c.net/showthread.php?t=91176>.

@note To get an idea on how to add your own weather effects, you may read
CryoniC's article about custom weather effects: <http://www.wc3c.net/showthread.php?t=67949>.
*/
native AddWeatherEffect takes rect where, integer effectID returns weathereffect

native RemoveWeatherEffect takes weathereffect whichEffect returns nothing

native EnableWeatherEffect takes weathereffect whichEffect, boolean enable returns nothing



native TerrainDeformCrater takes real x, real y, real radius, real depth, integer duration, boolean permanent returns terraindeformation

native TerrainDeformRipple takes real x, real y, real radius, real depth, integer duration, integer count, real spaceWaves, real timeWaves, real radiusStartPct, boolean limitNeg returns terraindeformation

native TerrainDeformWave takes real x, real y, real dirX, real dirY, real distance, real speed, real radius, real depth, integer trailTime, integer count returns terraindeformation

native TerrainDeformRandom takes real x, real y, real radius, real minDelta, real maxDelta, integer duration, integer updateInterval returns terraindeformation

native TerrainDeformStop takes terraindeformation deformation, integer duration returns nothing

native TerrainDeformStopAll takes nothing returns nothing



/**
Creates the special effect in point with coordinates (x;y) with Z = 0 using the
model file with a path modelName.

@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.
*/
native AddSpecialEffect takes string modelName, real x, real y returns effect

/**
Creates the special effect in the stated location where with Z = 0 using the
model file with a path modelName.

@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.
*/
native AddSpecialEffectLoc takes string modelName, location where returns effect

/**
Attaches the special effect to the attachment point attachPointName of the
target widget, using the model file with a path modelName.
*/
native AddSpecialEffectTarget takes string modelName, widget targetWidget, string attachPointName returns effect

native DestroyEffect takes effect whichEffect returns nothing



/**
@note No one knows what abilityString is supposed to be.
@bug Does nothing.
*/
native AddSpellEffect takes string abilityString, effecttype t, real x, real y returns effect

/**
@note No one knows what abilityString is supposed to be.
@bug Does nothing.
*/
native AddSpellEffectLoc takes string abilityString, effecttype t,location where returns effect

/**
Creates the special effect in point with coordinates (x;y) with Z = 0 using the
model file from the Object Editor field of type t from the ability, unit or
buff (works with all these types, though the name states it's ability-only
function) with raw code abilityId. If this field has more than one effect
inside, it will only create the first effect stated in the field, ignoring
all others.

@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.
*/
native AddSpellEffectById takes integer abilityId, effecttype t,real x, real y returns effect

/**
Creates the special effect in location where with Z = 0 using the model file
from the Object Editor field of type t from the ability, unit or buff (works
with all these types, though the name states it's ability-only function) with
raw code abilityId. If this field has more than one effect inside, it will only
create the first effect stated in the field, ignoring all others.

@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.
*/
native AddSpellEffectByIdLoc takes integer abilityId, effecttype t,location where returns effect

native AddSpellEffectTarget takes string modelName, effecttype t, widget targetWidget, string attachPoint returns effect

/**
Attaches the special effect to the attachment point attachPointName of the
target widget, using the model file from the Object Editor field of type t from
the ability, unit or buff (works with all these types, though the name states
it's ability-only function) with raw code abilityId. If this field has more than
one effect inside, it will only create the first effect stated in the field,
ignoring all others.
*/
native AddSpellEffectTargetById takes integer abilityId, effecttype t, widget targetWidget, string attachPoint returns effect


/**
Adds the lightning of type codeName between two points with coordinates (x1;y1)
and (x2;y2). The checkVisibility parameter allows to toggle lightning's
visibility in fog of war and black mask: true will force it to show in the fog
of war and black mask when false is the reverse operation. This function is
making Z coordinates of both lightning edges equal to 0.
*/
native AddLightning takes string codeName, boolean checkVisibility, real x1, real y1, real x2, real y2 returns lightning

/**
Adds the lightning of type codeName between two points with coordinates (x1;y1;z1)
and (x2;y2:z2). The checkVisibility parameter allows to toggle lightning's
visibility in fog of war and black mask: true will force it to show in the fog
of war and black mask when false is the reverse operation.
*/
native AddLightningEx takes string codeName, boolean checkVisibility, real x1, real y1, real z1, real x2, real y2, real z2 returns lightning

native DestroyLightning takes lightning whichBolt returns boolean

native MoveLightning takes lightning whichBolt, boolean checkVisibility, real x1, real y1, real x2, real y2 returns boolean

native MoveLightningEx takes lightning whichBolt, boolean checkVisibility, real x1, real y1, real z1, real x2, real y2, real z2 returns boolean

native GetLightningColorA takes lightning whichBolt returns real

native GetLightningColorR takes lightning whichBolt returns real

native GetLightningColorG takes lightning whichBolt returns real

native GetLightningColorB takes lightning whichBolt returns real

native SetLightningColor takes lightning whichBolt, real r, real g, real b, real a returns boolean


/**
@note No one knows what abilityString is supposed to be.
@bug Does nothing.
*/
native GetAbilityEffect takes string abilityString, effecttype t, integer index returns string

native GetAbilityEffectById takes integer abilityId, effecttype t, integer index returns string

/**
@note No one knows what abilityString is supposed to be.
@bug Does nothing.
*/
native GetAbilitySound takes string abilityString, soundtype t returns string

native GetAbilitySoundById takes integer abilityId, soundtype t returns string
