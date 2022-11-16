// Visual API

/**
@bug Does nothing (unknown, unused).
*/
native SetTerrainFog                takes real a, real b, real c, real d, real e returns nothing

native ResetTerrainFog              takes nothing returns nothing


/**
Unknown, unused.
*/
native SetUnitFog                   takes real a, real b, real c, real d, real e returns nothing

native SetTerrainFogEx              takes integer style, real zstart, real zend, real density, real red, real green, real blue returns nothing


/**
Displays a trigger message to player.

The text line fades out in the end.

@bug Changing x or y moves the entire text box, including previously displayed lines.

@note The text lines are bottom-left aligned: text continues to the right and new lines
continue upwards.

@note This is equivalent to `DisplayTimedTextToPlayer` with `duration` set to 4.

@note See: `DisplayTimedTextToPlayer`, `DisplayTimedTextFromPlayer`, `BlzDisplayChatMessage`.

@param toPlayer target player
@param x new text box position (default is 0, clamped to: 0.0-1.0)
@param y new text box position (default is 0, clamped to: 0.0-1.0)
@param message text (supports color codes)
*/
native DisplayTextToPlayer          takes player toPlayer, real x, real y, string message returns nothing


/**
Displays a trigger message to player with a custom display duration.

The text line fades out in the end.

@note See: `DisplayTextToPlayer` for the full description.
Also: `DisplayTimedTextFromPlayer`, `BlzDisplayChatMessage`.

@param toPlayer target player
@param x new text box position (default is 0, clamped to: 0.0-1.0)
@param y new text box position (default is 0, clamped to: 0.0-1.0)
@param duration text lifetime in seconds
@param message text (supports color codes)
*/
native DisplayTimedTextToPlayer     takes player toPlayer, real x, real y, real duration, string message returns nothing


/**
Displays a trigger message to *all* players but the first "%s" in the message will
be replaced by `GetPlayerName(toPlayer)`.

@bug Only the first "%s" will be replaced correctly. Following "%s" will be
printed as garbage or (v1.32.10, Lua) crash the game.

Using formatters like "%i" will also print garbage and following "%s" wont
work either.

See: [C stdlib printf documentation](https://cplusplus.com/reference/cstdio/printf/).

@note A better name for the parameter `toPlayer` would be `fromPlayer`.

@note See: `DisplayTextToPlayer` for the full description.
Also: `DisplayTimedTextToPlayer`, `BlzDisplayChatMessage`.

@param toPlayer this player's name will be used to replace the `%s` placeholder
@param x new text box position (default is 0, clamped to: 0.0-1.0)
@param y new text box position (default is 0, clamped to: 0.0-1.0)
@param duration text lifetime in seconds
@param message text (supports color codes), may contain only one `%s` placeholder
*/
native DisplayTimedTextFromPlayer   takes player toPlayer, real x, real y, real duration, string message returns nothing

/**
Clears all messages displayed via triggers. All messages will still show up in the message log, however.

@note This does not remove player chat messages.
*/
native ClearTextMessages            takes nothing returns nothing

native SetDayNightModels            takes string terrainDNCFile, string unitDNCFile returns nothing

native SetSkyModel                  takes string skyModelFile returns nothing


/**
Toggles user's input controls.

When disabled this includes:

- hide the cursor (you can still see UI on-hover effects with menu buttons and
even mouse-down animation on ability buttons)
- on-hover unit selection circles no longer show (cannot be overriden with
`EnableDragSelect`, `EnablePreSelect`, `EnableSelect`)
- disable all hotkeys (binds, abilities, minimap, menus like F10), only Alt+F4 continues to work

@param b `true` to enable control, `false` to disable
*/
native EnableUserControl            takes boolean b returns nothing


/**
Toggles the display of tooltips, other features unknown (v1.32.10).
Group hotkeys, selection etc. continue to work.

@param b `true` to enable tooltips, `false` to disable the display of tooltips.
*/
native EnableUserUI                 takes boolean b returns nothing


/**
Controls the ticking of the in-game day/night time.

@param b `true` to stop time ticking, `false` to enable time progression (default).
*/
native SuspendTimeOfDay             takes boolean b returns nothing


/**
Sets the speed of the in-game day/night time.

By default: `1.0` or 100%. `2.0` would make it twice as fast.

@param r new scaling factor

@bug A negative scaling factor is applied and the time ticks backwards until
00:00 is reached. Then the time freezes at 00:00, the day does not progress backwards.
*/
native SetTimeOfDayScale            takes real r returns nothing


/**
Returns the speed of the in-game day/night time (a scaling factor).
By default: `1.0` or 100%.
*/
native GetTimeOfDayScale            takes nothing returns real

/**
@bug If fadeDuration equals 0 the unit portrait always appears invisible.
*/
native ShowInterface                takes boolean flag, real fadeDuration returns nothing

native PauseGame                    takes boolean flag returns nothing


/**
@note See: `AddIndicator` (it is a more generic version as it takes a `widget`).
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

@note See: `UnitAddIndicator` (functionally equivalent to this widget version).

@param whichWidget The widget the indicator will be applied to.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha 0-255 opacity (value mod 256). Determining the transparency
of the indicator. `0` is total transparency, `255` is total opacity.
*/
native AddIndicator                 takes widget whichWidget, integer red, integer green, integer blue, integer alpha returns nothing

/**
Pings a spot on the minimap.

@param x horizontal world coordinate of the ping.
@param y vertical world coordinate of the ping.
@param duration duration of the ping in seconds.

@note This ping has the semantics of a "simple" ping (GUI/blizzard.j terminology).

@note As a "simple" ping, pings created with this function have a default shape of a rotating circle with 4 arrows
pointing inwards and periodically emitting a growing circle that fades out like a pulse. There is also a dot in the center.

@note This ping is neon green on default.

@note There can only be 16 pings at a time. When a new one is created but there are already 16,
the oldest will be deleted in favor of the new one. This includes user pings: user pings can be deleted by this function
and user pings can overwrite scripted pings.
*/
native PingMinimap                  takes real x, real y, real duration returns nothing

/**
Pings a spot on the minimap.

@param x horizontal world coordinate of the ping.
@param y vertical world coordinate of the ping.
@param duration duration of the ping in seconds.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param extraEffects When true, the ping will have the appearance of a "flashy" ping. Otherwise it will be a "simple" ping (see notes).

@note "Simple" pings (GUI/blizzard.j terminology) have a default shape of a rotating circle with 4 arrows
pointing inwards and periodically emitting a growing circle that fades out like a pulse. There is also a dot in the center.

@note "Flashy" pings (GUI/blizzard.j terminology) have the same shape as user-generated pings. On default, they first feature an exclamation mark
and a large circle growing and fading out before going into a stable state where smaller circles are periodically emitted growing and fading out
like a pulse and there is a static exclamation mark in the center.

@note Pings with red == 255 && green == 0 && blue == 0 (mod 256) have a special shape, appearing as "attack" or "warning" pings (GUI/blizzard.j terminology).
On default, if extraEffects is false, it is similar to "simple" pings but the rotating arrows are flying in from outside before getting attached to the circle.
On default, if extraEffects is true, it additionally briefly shows an exclamation mark when the ping vanishes (bug?).

@note There can only be 16 pings at a time. When a new one is created but there are already 16,
the oldest will be deleted in favor of the new one. This includes user pings: user pings can be deleted by this function
and user pings can overwrite scripted pings.
*/
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

@note Does not trigger (physical) player key events like `BlzTriggerRegisterPlayerKeyEvent`.
*/
native ForceUICancel                takes nothing returns nothing


/**
Opens the "Load game" menu where you can load a previous save.

@note Singleplayer only! This menu is disabled in multiplayer and nothing will happen.
*/
native DisplayLoadDialog            takes nothing returns nothing

/**
Sets the "alternative icon". You can display this icon for any unit via
`UnitSetUsesAltIcon`.

@note Only one icon can be the "alternative icon" but you can give each
player a different icon via `GetLocalPlayer`.
*/
native SetAltMinimapIcon            takes string iconPath returns nothing


/**
Toggles the "Restart Mission" button (found in: Menu (F10) -> End Game).

@note This button is only enabled in singleplayer (default),
you cannot enable it in multiplayer.

@param flag `true` to disable the button, `false` to allow game restarts by the player.
*/
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


/**
Returns the currently chosen player color display mode.

This is called "ally color mode" by the game (hotkey: Alt+A).

- `0` aka "Mode 1" (default):
    - Minimap: Player colors, youself are white
	- World: Unit colors same as player color
- `1` aka "Mode 2":
    - Minimap: Allies are teal, enemies are red, yourself are white
	- World: Unit colors same as player color
- `2` aka "Mode 3":
    - Minimap: Allies are teal, enemies are red, yourself are white
	- World: Allies are teal, enemies are red, own units are blue

@note See: `SetAllyColorFilterState`

@note This setting affects how a unit's "Art - Team Color" (WE name) is displayed.
If the models you use rely on this color to match player color,
you can choose to force state=0 with `SetAllyColorFilterState`.

@async
*/
native GetAllyColorFilterState      takes nothing returns integer


/**
Sets the player color display mode.

@note This is a player setting. Do not change it without a reason.

@bug You can set other states than 0-2, but they'll still display like state 0.

@note See: `GetAllyColorFilterState`

@param state new state (only 0, 1, 2 are valid).
See `GetAllyColorFilterState` for a description.
*/
native SetAllyColorFilterState      takes integer state returns nothing


/**
Returns `true` if the local player has enabled the display of creep camps on the minimap.

@note See: `SetCreepCampFilterState`, `GetAllyColorFilterState`

@async
*/
native GetCreepCampFilterState      takes nothing returns boolean

/**
Toggles minimap creep display.

The creep camps are shown as green/orange/red circles by default and there's a button
next to the minimap to toggle it while playing (hotkey: Alt+R).

@note See: `GetCreepCampFilterState`, `SetAllyColorFilterState`

@param state `true` to highlight camps, `false` to hide
*/
native SetCreepCampFilterState      takes boolean state returns nothing

native EnableMinimapFilterButtons   takes boolean enableAlly, boolean enableCreep returns nothing

/**
Sets the functionality of the rectangular unit multi-select.

"Drag Select" allows you to hold left-click to select multiple units by
expanding the green selection rectangle over the units.

@param state
If `true`, default game behavior (drag select is enabled).

If `false`, drag select is disabled. Only the first unit in the rectangle will
be selected (closest to the point where you first clicked the mouse).

Note that you can still select multiple units with Shift+Click even if drag
select is disabled.

@param ui
If `true`, render the visual indicator that shows the green rectangular selection area (default).
Units, that are not yet selected but are inside the rectangle,
have a semi-transparent green circle around them.

If `false`, the green rectangle is not rendered.
This has no effect on `state`, Drag Select can still work without the visual indicator.
*/
native EnableDragSelect             takes boolean state, boolean ui returns nothing


/**
Sets the functionality when you hover over a unit with your cursor.

@param state unknown
@param ui
If `true`, show semi-transparent green circle around the unit and the health bar etc.

If `false`, the green circle and the health bar is not shown.
The cursor still blinks green/yellow/red like when you hover over a unit.
The color depends on whether the unit is your own/ally/enemy.
*/
native EnablePreSelect              takes boolean state, boolean ui returns nothing


/**
Controls whether you can de/select any units and the green visual indicator.

@note
You can use `SelectUnit` and other functions to select the units for a player,
even when `state` is set to `false`.

The player cannot manually deselect any units they have control over (after `SelectUnit`).

@param state
If `true`, you can de/select units (default).

If `false`, deselects any currently selected units and disables your ability
to select any unit. Mouse clicks and group binds ("CTRL+1" then press "1")
don't work any more.
Drag select will not allow you to select too.

@param ui
If `true`, show the green selection indicator around selected units (default).

If `false`, no visual indicator is shown.
*/
native EnableSelect                 takes boolean state, boolean ui returns nothing

//============================================================================
//Machinima API
//============================================================================

/**

@bug (v1.32.10, Lua)
Enabling this mode without cinematic mode produces no visible differences. (TODO)

However, it shifts the rendered view towards north (without changing the
camera position) until disabled again.

See [test code](https://github.com/Luashine/wc3-test-maps/blob/master/BlzHideCinematicPanels.md)

@patch 1.32
*/
native BlzHideCinematicPanels                     takes boolean enable returns nothing

/**
@patch 1.32
*/
native SetPortraitLight             takes string portraitDNCFile returns nothing


/**
Toggles the rendering of terrain.

@param show `true` to render terrain, `false` no terrain (black background by default)

@note See: `BlzShowSkyBox`

@patch 1.32
*/
native BlzShowTerrain                              takes boolean show returns nothing

/**

@note See: `BlzShowTerrain`

@patch 1.32
*/
native BlzShowSkyBox                               takes boolean show returns nothing

/**
Does nothing (v1.32.10 without Battle.net App running), no files are created.

@note See: `BlzEndRecording`

@patch 1.32
*/
native BlzStartRecording                           takes integer fps returns nothing

/**
Does nothing (v1.32.10 without Battle.net App running), no files are created.

@note See: `BlzStartRecording`

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
