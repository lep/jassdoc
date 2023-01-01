// Effects API

/**
Creates a weather effect that is spatially limited to the specified area.

This creates a new object and returns its handle, to prevent leaks it must be destroyed
with `RemoveWeatherEffect` when no longer needed.

The weather effect is created initially disabled and must be turned on with `EnableWeatherEffect`.

**Example (Lua):** to create an "Ashenvale Heavy Rain" at map center:

```{.lua}
center = Rect(-1024, -1024, 1024, 1024)
weather = AddWeatherEffect(center, FourCC("RAhr"))
EnableWeatherEffect(weather, true)
```

@param where The rect where the weather will be visible.

@param effectID (Rawcode) Which weather preset to apply.

@note To understand more about weather effects nature, I advise to read
Ammorth's article about weather effects: <http://www.wc3c.net/showthread.php?t=91176>.

@note To get an idea on how to add your own weather effects, you may read
CryoniC's article about custom weather effects: <http://www.wc3c.net/showthread.php?t=67949>.

@note The weather effects are defined in `terrainart/weather.slk` in game files.
The [current default list is here](https://www.hiveworkshop.com/threads/how-to-create-random-weather.198658/post-1953519) (v1.32.10).
*/
native AddWeatherEffect takes rect where, integer effectID returns weathereffect


/**
Removes the weather effect (visually instant) and frees the handle.

@note See: `AddWeatherEffect`, `EnableWeatherEffect`.
*/
native RemoveWeatherEffect takes weathereffect whichEffect returns nothing


/**
Smoothly enables/disables the given weather effect.

@param whichEffect A handle of target weather effect.
@param enable `true` to enable, `false` to disable the effect.

@note See: `AddWeatherEffect`, `RemoveWeatherEffect`.
*/
native EnableWeatherEffect takes weathereffect whichEffect, boolean enable returns nothing



/**
Create a crater at the given coordinates.

@note To approximate the resulting height of a point `distance` units away from the
center point `(x, y)` you can use the following formula: `Cos(bj_PI/2 * distance / radius) * -depth`. See this [issue](https://github.com/lep/jassdoc/issues/31) for some more information.

@note Not every player might display those transformations due to graphics
settings. Thus reading data like terrain height might lead to async values.
See the other note on a way to compute an appropiate height to use instead.

@note Permanent terrain deformations are not present in saved game files.

@param x The x coordinate of the craters center.
@param y The y coordinate of the craters center.
@param radius The radius of the crater.
@param depth The depth of the crater.
@param duration The duration in milliseconds.
@param permanent Make the deformation permanent.
*/
native TerrainDeformCrater takes real x, real y, real radius, real depth, integer duration, boolean permanent returns terraindeformation

/**

@param duration The duration in milliseconds.

@note Permanent terrain deformations are not present in saved game files.
*/
native TerrainDeformRipple takes real x, real y, real radius, real depth, integer duration, integer count, real spaceWaves, real timeWaves, real radiusStartPct, boolean limitNeg returns terraindeformation

/**
@note Permanent terrain deformations are not present in saved game files.
*/
native TerrainDeformWave takes real x, real y, real dirX, real dirY, real distance, real speed, real radius, real depth, integer trailTime, integer count returns terraindeformation

/**

@param duration The duration in milliseconds.

@note Permanent terrain deformations are not present in saved game files.
*/
native TerrainDeformRandom takes real x, real y, real radius, real minDelta, real maxDelta, integer duration, integer updateInterval returns terraindeformation

native TerrainDeformStop takes terraindeformation deformation, integer duration returns nothing

native TerrainDeformStopAll takes nothing returns nothing



/**
Creates the special effect in point with coordinates (x;y) using the model file with a path modelName.
The altitude (Z) of the newly spawned effect is at the ground level, be it terrain, some pathable destructable or on top of water.
In other words, the effect's Z coordinate does not have to be 0.

@note To create an effect with an offset in relation to the ground before 1.30 patch, see <http://www.hiveworkshop.com/forums/1561722-post10.html>
@note In case of 1.30 patch or higher, use `BlzSetSpecialEffectZ` native.

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>
*/
native AddSpecialEffect takes string modelName, real x, real y returns effect

/**
Creates the special effect in the stated location using the model file with a path modelName.
The altitude (Z) of the newly spawned effect is at the ground level, be it terrain, some pathable destructable or on top of water.
In other words, the effect's Z coordinate does not have to be 0.

@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>.
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

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>.

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


/**
Changes(Set) the color of a special effect (tinting), using the specific player’s color, it will tint the effect on every part that it can be tinted.

@patch 1.29
*/
native BlzSetSpecialEffectColorByPlayer            takes effect whichEffect, player whichPlayer returns nothing

/**
Changes(Set) the color of a special effect (tinting), using R (RED) G (GREEN) B (BLUE) values, it will tint the effect on every part that it can be tinted.

@patch 1.29
*/
native BlzSetSpecialEffectColor                    takes effect whichEffect, integer r, integer g, integer b returns nothing

/**
Changes(Set) the alpha (transparency) of a special effect, the entire model will be made transparent based on the integer value.
*Integer Alpha goes from 0 to 100 (it equals percentage).*

@patch 1.29
*/
native BlzSetSpecialEffectAlpha                    takes effect whichEffect, integer alpha returns nothing

/**
Changes(Set) the scale of a special effect, the entire model will be scaled based on the scale value.

*Even though scale is a real (allows negative and positive numbers with decimals), it should be logically deduced that it shouldn’t be a negative value, object editor forces the minimum to be 0.10 (10% of the original size), it is not yet tested if it supports up to 0.01(1% of the original size).*

@patch 1.31
*/
native BlzSetSpecialEffectScale                    takes effect whichEffect, real scale returns nothing

/**
Changes(set) the X, Y and Z (altitude) coordinate (Cartesian System) of the current location of the special effect.

@note Z is not relative to terrain, it is absolute.

@patch 1.29
*/
native BlzSetSpecialEffectPosition                 takes effect whichEffect, real x, real y, real z returns nothing

/**
Sets the effect's absolute Z position (height). This native is functionally identical to BlzSetSpecialEffectZ.

@patch 1.29
*/
native BlzSetSpecialEffectHeight                   takes effect whichEffect, real height returns nothing

/**
Changes(set) the TimeScale (animation speed) of the passed special effect.

*TimeScale is a real, which means that it can be both negative and positive numbers with decimals, if you see the animation speed at 100.0 it will go at 100% speed, if you however set it to -100.0 it will go backwards and reset towards the beginning, however it can’t start at a negative value, if you want to reset the animation, you must pass it a negative value mid animation, else it will stand still.*

@patch 1.29
*/
native BlzSetSpecialEffectTimeScale                takes effect whichEffect, real timeScale returns nothing

/**
Changes(set) the time (how long the special effect lasts) of the passed special effect.

*TimeScale is a real, which means that it could be both negative and positive numbers with decimals, however it can’t be a negative value in this case.*

@patch 1.29
*/
native BlzSetSpecialEffectTime                     takes effect whichEffect, real time returns nothing

/**
Changes(set) the yaw, pitch and roll of the passed special effect.

*Yaw, pitch and roll are reals, which means that they can be both negative and positive numbers with decimals.*

@patch 1.29
*/
native BlzSetSpecialEffectOrientation              takes effect whichEffect, real yaw, real pitch, real roll returns nothing

/**
@patch 1.29
*/
native BlzSetSpecialEffectYaw                      takes effect whichEffect, real yaw returns nothing

/**
@patch 1.29
*/
native BlzSetSpecialEffectPitch                    takes effect whichEffect, real pitch returns nothing

/**
@patch 1.29
*/
native BlzSetSpecialEffectRoll                     takes effect whichEffect, real roll returns nothing

/**
@bug In 1.29 this native is bugged, it will set the X coordinate, but reset the Y and Z to where it was spawned in.

@patch 1.29
*/
native BlzSetSpecialEffectX                        takes effect whichEffect, real x returns nothing

/**
@bug In 1.29 this native is bugged, it will set the Y coordinate, but reset the X and Z to where it was spawned in.

@patch 1.29
*/
native BlzSetSpecialEffectY                        takes effect whichEffect, real y returns nothing

/**
Sets the effect's absolute Z position (height). 

@note Before 1.29 there was no direct way to set a special effect's height. The following trick was used as a workaround:

    // Creates a temporary platform in the air, the special effect will be put on top of it:
    set tempDestr = CreateDestructableZ('OTis', x, y, z, 0, 1, 0)
    // Effect spawns on top of platform
    call DestroyEffect(AddSpecialEffect(effectPath, x, y))
    // Remove platform immediately, only the effect will remain visible for its life duration
    call RemoveDestructable(tempDestr)

@bug In 1.29 this native is bugged, it will set the Z coordinate, but reset the X and Y to where it was spawned in.

@patch 1.29
*/
native BlzSetSpecialEffectZ                        takes effect whichEffect, real z returns nothing

/**
Changes(set) the current location of the special effect into the passed location.

@patch 1.29
*/
native BlzSetSpecialEffectPositionLoc              takes effect whichEffect, location loc returns nothing

/**
Get the X coordinate (Cartesian System) of the current location of the special effect.

@async
@patch 1.29
*/
native BlzGetLocalSpecialEffectX                   takes effect whichEffect returns real

/**
Get the Y coordinate (Cartesian System) of the current location of the special effect.

@async
@patch 1.29
*/
native BlzGetLocalSpecialEffectY                   takes effect whichEffect returns real

/**
Get the absolute Z coordinate (altitude)(Cartesian System) of the current location of the special effect.

@async
@patch 1.29
*/
native BlzGetLocalSpecialEffectZ                   takes effect whichEffect returns real

/**
Clears all subanimations (tags) of the special effect. It does not affect normal animations.

**Example usage of subanimations:**

    // if you play anim attack it becomes attack slam:
    call BlzSpecialEffectAddSubAnimation(fx, SUBANIM_TYPE_SLAM)
    call BlzPlaySpecialEffect(fx, ANIM_TYPE_SPELL)
    call BlzSpecialEffectRemoveSubAnimation(fx, SUBANIM_TYPE_SLAM)

**Examples of animations, animation names:**

    stand | birth | death | decay | dissipate | walk | attack | morph | sleep | spell | portrait

**Examples of subanimations (tags), subanimation names:**

    first | second | third | fourth | fifth | defend | channel | slam | victory | throw | spin |
    ready | upgrade | lumber | gold | work | talk | swim | flesh | entangle | chainlightning | rooted |
    eattree | berserk | spiked | light | moderate | severe | critical | small | medium | large | alternateex |
    looping | wounded | fast | turn | left | right | fire | one | two | three | four | five | fill |
    puke | drain | flail | hit | off | complete

@patch 1.30
*/
native BlzSpecialEffectClearSubAnimations          takes effect whichEffect returns nothing

/**
Clears a specific subanimation (tag) of a specified special effect. (It does not affect normal animations).

@patch 1.30
*/
native BlzSpecialEffectRemoveSubAnimation          takes effect whichEffect, subanimtype whichSubAnim returns nothing

/**
Adds(set) a specific subanimation (tag) to a specified special effect.

@patch 1.30
*/
native BlzSpecialEffectAddSubAnimation             takes effect whichEffect, subanimtype whichSubAnim returns nothing

/**
Plays a specific subanimation (tag) on a specified special effect.

@patch 1.30
*/
native BlzPlaySpecialEffect                        takes effect whichEffect, animtype whichAnim returns nothing

/**
Plays a specific subanimation (tag) on a specified special effect at a specific timeScale (speed).

1. *Overrides the currently playing animation/subanimation.*
2. *TimeScale is a real, meaning that it can be both negative and positive numbers with decimals, there are examples in which you can use negative numbers mid animation to make it go backwards, however in this case it starts at 0 meaning that it can’t be negative.*

@patch 1.30
*/
native BlzPlaySpecialEffectWithTimeScale           takes effect whichEffect, animtype whichAnim, real timeScale returns nothing

/**
Returns the string representation of the name of the animation. `animtype` is a handle of the animation type.

@patch 1.30
*/
native BlzGetAnimName                              takes animtype whichAnim returns string

/**
@patch 1.31
*/
native BlzGetSpecialEffectScale                    takes effect whichEffect returns real

/**
@patch 1.31
*/
native BlzSetSpecialEffectMatrixScale              takes effect whichEffect, real x, real y, real z returns nothing

/**
@patch 1.31
*/
native BlzResetSpecialEffectMatrix                 takes effect whichEffect returns nothing
