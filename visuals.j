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


/**
@note See: `AddIndicator` (it is a more generic version as it takes a `widget`)
*/
native UnitAddIndicator             takes unit whichUnit, integer red, integer green, integer blue, integer alpha returns nothing

/**
Adds a blinking circle around widget with the color (red,green,blue,alpha).
The circle blinks twice. This function is commonly used for cinematic modes
and is seen in `TransmissionFromUnitWithNameBJ`.

@note The size of the indicator depends on a widget's selection size. To modify
this, you must edit the object editor field of the widget listed as "Art - Selection Size".

The indicator is shown below the unit selection.
If the unit is currently selected, the blinking indicator will be practically
hidden by the selection circle. For more see `SetImageType` description.

@note See: `UnitAddIndicator` (functionally equivalent to this widget version)

@param whichWidget The widget the indicator will be applied to.
@param red 0-255 red color (value mod 256)
@param green 0-255 green color (value mod 256)
@param blue 0-255 blue color (value mod 256)
@param alpha 0-255 opacity (value mod 256). Determining the transparency
of the indicator. `0` is total transparency, `255` is total opacity.
*/
native AddIndicator                 takes widget whichWidget, integer red, integer green, integer blue, integer alpha returns nothing

native PingMinimap                  takes real x, real y, real duration returns nothing

native PingMinimapEx                takes real x, real y, real duration, integer red, integer green, integer blue, boolean extraEffects returns nothing


/**
@patch 1.32
*/
native CreateMinimapIconOnUnit      takes unit whichUnit, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon

/**
@patch 1.32
*/
native CreateMinimapIconAtLoc       takes location where, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon

/**
@patch 1.32
*/
native CreateMinimapIcon            takes real x, real y, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon

/**
@patch 1.32
*/
native SkinManagerGetLocalPath      takes string key returns string

/**
@patch 1.32
*/
native DestroyMinimapIcon           takes minimapicon pingId returns nothing

/**
@patch 1.32
*/
native SetMinimapIconVisible        takes minimapicon whichMinimapIcon, boolean visible returns nothing

/**
@patch 1.32
*/
native SetMinimapIconOrphanDestroy  takes minimapicon whichMinimapIcon, boolean doDestroy returns nothing

native EnableOcclusion              takes boolean flag returns nothing

native SetIntroShotText             takes string introText returns nothing

native SetIntroShotModel            takes string introModelPath returns nothing

native EnableWorldFogBoundary       takes boolean b returns nothing

native PlayModelCinematic           takes string modelName returns nothing

native PlayCinematic                takes string movieName returns nothing

/**
Emulates a key press within the game. Seems to only work with latin alphabet, only for printable ASCII characters.

@note See `ForceUICancel` for limitations and bugs. Most importantly, the outcome is affected by local player's hotkey layout.
*/
native ForceUIKey                   takes string key returns nothing

/**
Emulates an ESCAPE key press internally, used to interact with UI, e.g. close F10 menu.

@bug Does not always work as expected if you use it to "Cancel" something on behalf of a player, like cancel research in the current building. Since it always sends the Escape key, it will break if hotkey layout was changed from classic to grid/custom in game settings. Explanation:

1. OldPlayer plays with classic hotkey layout, the Cancelling abilities are bound to Escape.
2. ModernPlayer plays with grid layout, the Cancelling abilities' hotkey depends on their position but it's usually V.
3. ForceUICancel() is executed for both players
4. OldPlayer executes a Cancel ability, nothing happens to ModernPlayer
5. The game doesn't desync because it thinks OldPlayer really pressed that key, and even though ModernPlayer did "press" it too, he didn't trigger Cancel for his unit.

@note Does not trigger (physical) player key events like `BlzTriggerRegisterPlayerKeyEvent`
*/
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

//============================================================================
//Machinima API
//============================================================================

/**
@patch 1.32
*/
native BlzHideCinematicPanels                     takes boolean enable returns nothing

/**
@patch 1.32
*/
native SetPortraitLight             takes string portraitDNCFile returns nothing


/**
@patch 1.32
*/
native BlzShowTerrain                              takes boolean show returns nothing

/**
@patch 1.32
*/
native BlzShowSkyBox                               takes boolean show returns nothing

/**
@patch 1.32
*/
native BlzStartRecording                           takes integer fps returns nothing

/**
@patch 1.32
*/
native BlzEndRecording                             takes nothing returns nothing

/**
Toggle team glow on whichUnit.
Will remove Hero glowing team color when set to false.

@param whichUnit Target unit (handle).
@param show Boolean to show/hide the team glow.
@patch 1.32
*/
native BlzShowUnitTeamGlow                         takes unit whichUnit, boolean show returns nothing

// Skin

/**
@patch 1.32
*/
native BlzGetUnitSkin                                 takes unit whichUnit returns integer

/**
@patch 1.32
*/
native BlzGetItemSkin                                 takes item whichItem returns integer
// native BlzGetDestructableSkin                         takes destructable whichDestructable returns integer

/**
Replaces a unit's model with the unit's model referenced by the skinId.
`BlzSetUnitSkin(whichUnit, 'hfoo')` will replace whichUnit model with the footman one.
Scale from the unit referenced by the skinId is applied to whichUnit.
SoundSet from the unit referenced by the skinId is applied to whichUnit.

@param whichUnit The function will modify this unit's model.
@param skinId The function will apply the skinId model to whichUnit.

@note Upon function call, all attachment visual effect are removed from whichUnit.
@patch 1.32
*/
native BlzSetUnitSkin                                 takes unit whichUnit, integer skinId returns nothing

/**
@patch 1.32
*/
native BlzSetItemSkin                                 takes item whichItem, integer skinId returns nothing
// native BlzSetDestructableSkin                         takes destructable whichDestructable, integer skinId returns nothing


/**
@patch 1.32
*/
native BlzCreateItemWithSkin                       takes integer itemid, real x, real y, integer skinId returns item

/**
Creates a unit with the model from the unit referenced by the skinId.
`BlzCreateUnitWithSkin(players[0], 'hpea', 0, 0, 270, 'hfoo')` will create a peasant with a footman model.
Scale from the unit referenced by the skinId is applied to whichUnit.
SoundSet from the unit referenced by the skinId is applied to whichUnit.

@param id The owner of the unit.
@param unitid The rawcode of the unit.
@param x The x-coordinate of the unit.
@param y The y-coordinate of the unit.
@param face Unit facing in degrees.
@param skinId The function will apply the skinId model to the unit created.
@patch 1.32
*/
native BlzCreateUnitWithSkin                       takes player id, integer unitid, real x, real y, real face, integer skinId returns unit

/**
@patch 1.32
*/
native BlzCreateDestructableWithSkin               takes integer objectid, real x, real y, real face, real scale, integer variation, integer skinId returns destructable

/**
@patch 1.32
*/
native BlzCreateDestructableZWithSkin              takes integer objectid, real x, real y, real z, real face, real scale, integer variation, integer skinId returns destructable

/**
@patch 1.32
*/
native BlzCreateDeadDestructableWithSkin           takes integer objectid, real x, real y, real face, real scale, integer variation, integer skinId returns destructable

/**
@patch 1.32
*/
native BlzCreateDeadDestructableZWithSkin          takes integer objectid, real x, real y, real z, real face, real scale, integer variation, integer skinId returns destructable
