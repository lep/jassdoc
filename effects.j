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

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>
*/
native AddSpecialEffect takes string modelName, real x, real y returns effect

/**
Creates the special effect in the stated location where with Z = 0 using the
model file with a path modelName.

@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>
*/
native AddSpecialEffectLoc takes string modelName, location where returns effect

/**
Attaches the special effect to the attachment point attachPointName of the
target widget, using the model file with a path modelName.

Upon creation, the effect will play its "birth" animation followed by its "stand" animation (once the birth animation has finished). If the model does not have animations, it will show up the way it appears by default. The effect will last indefinitely unless it is destroyed, even if the model seems to disappear. To destroy an effect, see DestroyEffect.


@param modelName The path of the model. Use double backslashes when specifying
a directory, rather than single backslashes. See AddSpecialEffect for an example.

@param targetWidget The widget to attach the effect to.

@param  attachPointName The attachment point of the widget where the effect will
be placed. Attachment points are points in a model that can be referenced to as
areas for effects to be attached, whether it be from a spell or this function.
A list of common attachment points in in-game Warcraft 3 models can be seen below.
If the attachment point does not exist, it will attach the effect to the model's origin.

@note Strings such as "Large" and "Medium" affect effects' sizes on the widget
it is attached to. You can add or remove these by going to the object editor and
modifying "Art - Required Animation Names - Attachments" for a particular unit
you are attaching effects to. 

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>

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
and (x2;y2;z2). The checkVisibility parameter allows to toggle lightning's
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
@pure
*/
native GetAbilityEffect takes string abilityString, effecttype t, integer index returns string

/**
@pure
*/
native GetAbilityEffectById takes integer abilityId, effecttype t, integer index returns string

/**
@note No one knows what abilityString is supposed to be.
@bug Does nothing.
@pure
*/
native GetAbilitySound takes string abilityString, soundtype t returns string

/**
@pure
*/
native GetAbilitySoundById takes integer abilityId, soundtype t returns string


native BlzSetSpecialEffectColorByPlayer            takes effect whichEffect, player whichPlayer returns nothing
native BlzSetSpecialEffectColor                    takes effect whichEffect, integer r, integer g, integer b returns nothing
native BlzSetSpecialEffectAlpha                    takes effect whichEffect, integer alpha returns nothing
native BlzSetSpecialEffectScale                    takes effect whichEffect, real scale returns nothing
native BlzSetSpecialEffectPosition                 takes effect whichEffect, real x, real y, real z returns nothing
native BlzSetSpecialEffectHeight                   takes effect whichEffect, real height returns nothing
native BlzSetSpecialEffectTimeScale                takes effect whichEffect, real timeScale returns nothing
native BlzSetSpecialEffectTime                     takes effect whichEffect, real time returns nothing
native BlzSetSpecialEffectOrientation              takes effect whichEffect, real yaw, real pitch, real roll returns nothing
native BlzSetSpecialEffectYaw                      takes effect whichEffect, real yaw returns nothing
native BlzSetSpecialEffectPitch                    takes effect whichEffect, real pitch returns nothing
native BlzSetSpecialEffectRoll                     takes effect whichEffect, real roll returns nothing
native BlzSetSpecialEffectX                        takes effect whichEffect, real x returns nothing
native BlzSetSpecialEffectY                        takes effect whichEffect, real y returns nothing
native BlzSetSpecialEffectZ                        takes effect whichEffect, real z returns nothing
native BlzSetSpecialEffectPositionLoc              takes effect whichEffect, location loc returns nothing

/**
@async
*/
native BlzGetLocalSpecialEffectX                   takes effect whichEffect returns real

/**
@async
*/
native BlzGetLocalSpecialEffectY                   takes effect whichEffect returns real

/**
@async
*/
native BlzGetLocalSpecialEffectZ                   takes effect whichEffect returns real
native BlzSpecialEffectClearSubAnimations          takes effect whichEffect returns nothing
native BlzSpecialEffectRemoveSubAnimation          takes effect whichEffect, subanimtype whichSubAnim returns nothing
native BlzSpecialEffectAddSubAnimation             takes effect whichEffect, subanimtype whichSubAnim returns nothing
native BlzPlaySpecialEffect                        takes effect whichEffect, animtype whichAnim returns nothing
native BlzPlaySpecialEffectWithTimeScale           takes effect whichEffect, animtype whichAnim, real timeScale returns nothing

native BlzGetSpecialEffectScale                    takes effect whichEffect returns real
native BlzSetSpecialEffectMatrixScale              takes effect whichEffect, real x, real y, real z returns nothing
native BlzResetSpecialEffectMatrix                 takes effect whichEffect returns nothing
