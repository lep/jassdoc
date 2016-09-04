// Visual API

/**
@bug Does nothing
*/
native SetTerrainFog                takes real a, real b, real c, real d, real e returns nothing

native ResetTerrainFog              takes nothing returns nothing



native SetUnitFog                   takes real a, real b, real c, real d, real e returns nothing

native SetTerrainFogEx              takes integer style, real zstart, real zend, real density, real red, real green, real blue returns nothing

native DisplayTextToPlayer          takes player toPlayer, real x, real y, string message returns nothing

native DisplayTimedTextToPlayer     takes player toPlayer, real x, real y, real duration, string message returns nothing

/**
Displays the message to *all* players but the first "%s" in the message will
be replaced by `GetPlayerName(toPlayer)`.

@bug Only the first "%s" will be replaced correctly. Following "%s" will be
printed as garbage.

@bug Using formatters like "%i" will also print garbage and following "%s" wont
work either.

@note A better name for the parameter `toPlayer` would be `fromPlayer`.
*/
native DisplayTimedTextFromPlayer   takes player toPlayer, real x, real y, real duration, string message returns nothing

/**
Clears all messages displayed via triggers. All messages will still show up in the message log, however.

@note This does not remove player chat messages. 
*/
native ClearTextMessages            takes nothing returns nothing

native SetDayNightModels            takes string terrainDNCFile, string unitDNCFile returns nothing

native SetSkyModel                  takes string skyModelFile returns nothing

native EnableUserControl            takes boolean b returns nothing

native EnableUserUI                 takes boolean b returns nothing

native SuspendTimeOfDay             takes boolean b returns nothing

native SetTimeOfDayScale            takes real r returns nothing

native GetTimeOfDayScale            takes nothing returns real

native ShowInterface                takes boolean flag, real fadeDuration returns nothing

native PauseGame                    takes boolean flag returns nothing

native UnitAddIndicator             takes unit whichUnit, integer red, integer green, integer blue, integer alpha returns nothing

/**
Adds a blinking circle around widget with the color (red,green,blue,alpha).
The circle blinks twice. This function is commonly used for cinematic modes
and is seen in `TransmissionFromUnitWithNameBJ`.

@note The size of the indicator depends on a unit's selection size. To modify
this, you must edit the object editor field of the unit listed as "Art - Selection Size".

@param whichWidget The widget the indicator will be applied to.
@param red An integer from 0-255 determining the amount of red color in the indicator.
@param green An integer from 0-255 determining the amount of green color in the indicator.
@param blue An integer from 0-255 determining the amount of blue color in the indicator.
@param alpha An integer from 0-255 determining the transparency of the indicator. A value of 0 is complete transparency while a value of 255 is complete opacity.
*/
native AddIndicator                 takes widget whichWidget, integer red, integer green, integer blue, integer alpha returns nothing

native PingMinimap                  takes real x, real y, real duration returns nothing

native PingMinimapEx                takes real x, real y, real duration, integer red, integer green, integer blue, boolean extraEffects returns nothing

native EnableOcclusion              takes boolean flag returns nothing

native SetIntroShotText             takes string introText returns nothing

native SetIntroShotModel            takes string introModelPath returns nothing

native EnableWorldFogBoundary       takes boolean b returns nothing

native PlayModelCinematic           takes string modelName returns nothing

native PlayCinematic                takes string movieName returns nothing

native ForceUIKey                   takes string key returns nothing

native ForceUICancel                takes nothing returns nothing

native DisplayLoadDialog            takes nothing returns nothing

/**
Sets the "alternative icon". You can display this icon for any unit via
`UnitSetUsesAltIcon`.

@note Only one icon can be the "alternative icon" but you can give each
player a different icon via `GetLocalPlayer`.
*/
native SetAltMinimapIcon            takes string iconPath returns nothing

native DisableRestartMission        takes boolean flag returns nothing



native CreateTextTag                takes nothing returns texttag

native DestroyTextTag               takes texttag t returns nothing

native SetTextTagText               takes texttag t, string s, real height returns nothing

native SetTextTagPos                takes texttag t, real x, real y, real heightOffset returns nothing

native SetTextTagPosUnit            takes texttag t, unit whichUnit, real heightOffset returns nothing

native SetTextTagColor              takes texttag t, integer red, integer green, integer blue, integer alpha returns nothing

native SetTextTagVelocity           takes texttag t, real xvel, real yvel returns nothing

native SetTextTagVisibility         takes texttag t, boolean flag returns nothing

native SetTextTagSuspended          takes texttag t, boolean flag returns nothing

native SetTextTagPermanent          takes texttag t, boolean flag returns nothing

native SetTextTagAge                takes texttag t, real age returns nothing

native SetTextTagLifespan           takes texttag t, real lifespan returns nothing

native SetTextTagFadepoint          takes texttag t, real fadepoint returns nothing



native SetReservedLocalHeroButtons  takes integer reserved returns nothing

native GetAllyColorFilterState      takes nothing returns integer

native SetAllyColorFilterState      takes integer state returns nothing

native GetCreepCampFilterState      takes nothing returns boolean

native SetCreepCampFilterState      takes boolean state returns nothing

native EnableMinimapFilterButtons   takes boolean enableAlly, boolean enableCreep returns nothing

native EnableDragSelect             takes boolean state, boolean ui returns nothing

native EnablePreSelect              takes boolean state, boolean ui returns nothing

native EnableSelect                 takes boolean state, boolean ui returns nothing
