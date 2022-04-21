

/**
Controls selection settings globally: enables/disables selection of units, and visibility of selection circles

@param enableSelection true to enable, false to disable selection
@param enableSelectionCircle true to show, false to hide selection circles on units and doodads

@patch 1.31
*/
native BlzEnableSelections                         takes boolean enableSelection, boolean enableSelectionCircle returns nothing

/**
Returns whether unit selection is enabled (a global setting, see `BlzEnableSelections`)

@patch 1.31
*/
native BlzIsSelectionEnabled                       takes nothing returns boolean

/**
Returns whether unit selection circles are shown (a global setting, see `BlzEnableSelections`)

@patch 1.31
*/
native BlzIsSelectionCircleEnabled                 takes nothing returns boolean

/**
@patch 1.31
*/
native BlzCameraSetupApplyForceDurationSmooth      takes camerasetup whichSetup, boolean doPan, real forcedDuration, real easeInDuration, real easeOutDuration, real smoothFactor returns nothing

/**
Enable or disable the three green arrows when right-clicking on ground.

@patch 1.31
*/
native BlzEnableTargetIndicator                    takes boolean enable returns nothing

/**
Check if the the three green arrows when right-clicking on ground is shown or not.

@patch 1.31
*/
native BlzIsTargetIndicatorEnabled                 takes nothing returns boolean


/**
Get a `framehandle` by specifying a specific `originframetype` and index (in most cases it should be 0 (first index), however it can go above 0 when using originframetypes such as `ORIGIN_FRAME_HERO_BUTTON`)

The one with indices above 0 are:

	// The ability buttons at the right bottom corner
    ORIGIN_FRAME_COMMAND_BUTTON <0 to 11>
	// The clickable hero icons at the left of the screen
    ORIGIN_FRAME_HERO_BUTTON <0 to 6>
	// See above for the following:
    ORIGIN_FRAME_HERO_HP_BAR <0 to 6>
    ORIGIN_FRAME_HERO_MANA_BAR <0 to 6>
    ORIGIN_FRAME_HERO_BUTTON_INDICATOR <0 to 6>
	// Item inventory buttons
    ORIGIN_FRAME_ITEM_BUTTON <0 to 5>
	// The buttons altering the minimap
    ORIGIN_FRAME_MINIMAP_BUTTON
	// Indices:
	// 0 = Menu
	// 1 = Allies
	// 2 = Log
	// 3 = Quest
    ORIGIN_FRAME_SYSTEM_BUTTON <0 to 3> 

Here is a basic example that creates a custom timerdialog window:

    set GameUI = BlzGetOriginFrame(ORIGIN_FRAME_GAME_UI, 0)
    set UIMain = BlzCreateFrame("TimerDialog", GameUI, 0, 0)
    call BlzFrameSetPoint(UIMain, FRAMEPOINT_CENTER, GameUI, FRAMEPOINT_CENTER, 0.25, 0.055)
    call BlzFrameSetSize(UIMain, 0.3, 0.7)

*Take a look at the .fdf files in the game’s CASC or point 1.3 (refer to this document’s table of contents for reference) it should give you some ideas.*

@note The first time a Frame enters the map's script it takes a handleId.

@note This is up for edition, this native is lacking a more in-depth explanation. For example a list of all of the originframetypes, and their possible indexes.

@patch 1.31
@param index to high values will return the frame from the last valid Index
*/
native BlzGetOriginFrame                           takes originframetype frameType, integer index returns framehandle

/**
Disabling Auto Position will prevent the game using default positions for changed hidden frames as soon the reappear/their state is changed.

@patch 1.31
*/
native BlzEnableUIAutoPosition                     takes boolean enable returns nothing

/**
Hides/Shows most of the default in-game UI.
Unaffected: Mouse, Command Buttons, Chat, Messages, TimerDialog, Multiboard, Leaderboard and ConsoleUIBackdrop


(De)Activades some auto-repositioning of default frames (see: `BlzEnableUIAutoPosition`)

@patch 1.31
*/
native BlzHideOriginFrames                         takes boolean enable returns nothing

/**
Loads in a TOCFile, to add/define Frame-Blueprints or Localized Strings
A TOC file contains a list, Each line is a path to a fdf (not case sensitve)

@bug The TOC needs to end with one or two empty lines

@patch 1.31
*/
native BlzLoadTOCFile                              takes string TOCFile returns boolean

/**
Create a new Frame using a Frame-BluePrint name (fdf) as child of owner.
BluePrint needs to be loaded over TOC & fdf.
Owner and BluePrint have to be from the Frame family.
Can only create rootFrames (not subFrames).
Created Frames are stored into the game's Frame-Storage, `BlzGetFrameByName(name, createContext)`. Overwrites occupied slots.

@patch 1.31
*/
native BlzCreateFrame                              takes string name, framehandle owner, integer priority, integer createContext returns framehandle

/**
Like `BlzCreateFrame` but for the SimpleFrame family, Frame "SIMPLExxxx".

@note Only Frames loaded by used tocs are valid names.

@patch 1.31
*/
native BlzCreateSimpleFrame                        takes string name, framehandle owner, integer createContext returns framehandle

/**
Create & Define a new (Simple)Frame.
Can use a root-(Simple)Frame-BluePrint with inherits, when that is done it needs to be a loaded BluePrint.

@patch 1.31
*/
native BlzCreateFrameByType                        takes string typeName, string name, framehandle owner, string inherits, integer createContext returns framehandle

/**
@patch 1.31
*/
native BlzDestroyFrame                             takes framehandle frame returns nothing

/**
Unbinds a point of FrameA and places it relative to a point of FrameB.
When FrameB moves FrameA's point will keep this rule and moves with it.

Each point of a frame can be placed to one point.
By placing multiple points of one Frame a Size is enforced.

@patch 1.31
*/
native BlzFrameSetPoint                            takes framehandle frame, framepointtype point, framehandle relative, framepointtype relativePoint, real x, real y returns nothing

/**
Set frame absolute x,y position with framepointtype.
Coords are for the 4:3 Screen

    |0.0/0.6           0.8/0.6|
    |                         |
    |         0.4/0.3         |
    |                         |
    |0.0/0.0           0.8/0.0|

0.0/0.0 is bottomLeft (Minimap)
0.8/0.6 is TopRight (UpkeepCost)
In widescreen format one can go further left with -x or further right with x > 0.8
Only some Frames and their Children/Offspring can leave 4:3.
SimpleFrames, Leaderboard, TimerDialog, Multiboard, ConsoleUIBackdrop

@param point framepointtype is a point, position of which you set to move the frame relatively to it.

@patch 1.31
*/
native BlzFrameSetAbsPoint                         takes framehandle frame, framepointtype point, real x, real y returns nothing

/**
Unbinds all points of frame.
Useful to move frames with the next SetPoint.

@patch 1.31
*/
native BlzFrameClearAllPoints                      takes framehandle frame returns nothing

/**
Example:

    BlzHideOriginFrames(true)
    BlzFrameSetAllPoints(BlzGetOriginFrame(ORIGIN_FRAME_WORLD_FRAME, 0), BlzGetOriginFrame(ORIGIN_FRAME_GAME_UI, 0))

@patch 1.31
@param frame the frame moved/resized
*/
native BlzFrameSetAllPoints                        takes framehandle frame, framehandle relative returns nothing

/**
Sets visibility of a frame and its children.

@param visible true is visible, false is invisible

@patch 1.31
*/
native BlzFrameSetVisible                          takes framehandle frame, boolean visible returns nothing

/**
Returns visibility status of frame.

@param frame Target frame

@async
@patch 1.31
*/
native BlzFrameIsVisible                           takes framehandle frame returns boolean


/**
Requires a string for the frame name that you want to retrieve (get), and an integer (which in most cases should be 0) that specifies the index of the frame that you want to get (for example for inventory slots you have 6, from 0-5).

Read from the internal Frame-Storage.
The first time a Frame enters the map's script it takes a handleId.

Example: `BlzGetFrameByName("SimpleHeroLevelBar", 0)`

@note Refer to fdf files for frame names

@patch 1.31
*/
native BlzGetFrameByName                           takes string name, integer createContext returns framehandle

/**
Returns the string representation of frame name.

Inherited Frames lose their Name.
SimpleFrames return an empty String.

@param frame A handle to frame

@patch 1.31
*/
native BlzFrameGetName                             takes framehandle frame returns string

/**
Ignores visibility. Triggers `FRAMEEVENT_CONTROL_CLICK`

@patch 1.31
*/
native BlzFrameClick                               takes framehandle frame returns nothing

/**
Supports Warcraft 3 formatting codes:

* Colors (`|cffffcc00`)
* Multiple lines (`|n`, `\n`)

@patch 1.31
*/
native BlzFrameSetText                             takes framehandle frame, string text returns nothing

/**
Returns(Get) the text of that frame. For user input frames this text probably differs between them. For some frames the child contains the Text.

@async
@patch 1.31
*/
native BlzFrameGetText                             takes framehandle frame returns string

/**
Start a NewLine and add text (TEXTAREA).
@patch 1.31
*/
native BlzFrameAddText                             takes framehandle frame, string text returns nothing

/**
@patch 1.31
*/
native BlzFrameSetTextSizeLimit                    takes framehandle frame, integer size returns nothing

/**
@patch 1.31
*/
native BlzFrameGetTextSizeLimit                    takes framehandle frame returns integer

/**
Changes text color of the frame. SimpleFrames only.

@param color Four byte integer of the form 0xaarrggbb. You can also use
`BlzConvertColor` to create such an integer.

@patch 1.31
*/
native BlzFrameSetTextColor                        takes framehandle frame, integer color returns nothing

/**
@patch 1.31
*/
native BlzFrameSetFocus                            takes framehandle frame, boolean flag returns nothing

/**
@patch 1.31
*/
native BlzFrameSetModel                            takes framehandle frame, string modelFile, integer cameraIndex returns nothing

/**
Turns on/off Interactivity/Events of frame
A disabled frame is transparent to the mouse (can click on things behind it) and can have a different color/texture/frame than in enabled state.
The frame's Tooltip is still shown on hover.
(false) Removes KeyboardFocus.

@patch 1.31
*/
native BlzFrameSetEnable                           takes framehandle frame, boolean enabled returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameGetEnable                           takes framehandle frame returns boolean

/**
Affects child-Frames, when they don't have an own Alpha.

@param alpha 0 to 255
@patch 1.31
*/
native BlzFrameSetAlpha                            takes framehandle frame, integer alpha returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameGetAlpha                            takes framehandle frame returns integer

/**
@patch 1.31
*/
native BlzFrameSetSpriteAnimate                    takes framehandle frame, integer primaryProp, integer flags returns nothing

/**
Overwrittes some fdf setup.

@patch 1.31

@param flag texture fill setting: 0 to stretch, 1 to tile (BACKDROP)
@param blend use transparency

*/
native BlzFrameSetTexture                          takes framehandle frame, string texFile, integer flag, boolean blend returns nothing

/**
Affects child-Frames, when they don't have an own Scale.
@patch 1.31
*/
native BlzFrameSetScale                            takes framehandle frame, real scale returns nothing

/**
Frame tooltip is visible when hovered with the mouse. Otherwise tooltip will be hidden.

tooltip is limited to 4:3, but not it's children.
SimpleFrame tooltips are not hidden with this call.
frame and tooltip have to be from the same Family (Frames/SimpleFrames).
tooltip can only serve one frame.
It's not possible to undo this

@bug Crashes the game, on hover, when done twice (same pair).
@bug Frames should not be used as tooltips for simple Frames (Crash on PTR 1.31).

@patch 1.31
*/
native BlzFrameSetTooltip                          takes framehandle frame, framehandle tooltip returns nothing

/**
The mouse cursor is forced into the frame and can not leave it. New cages (true) will overwrite old ones. Some frames can not be used to imprison the mouse.

@param enable Enable mouse cage

@patch 1.31
*/
native BlzFrameCageMouse                           takes framehandle frame, boolean enable returns nothing

/**
Sets the current Frame Value. Only for FrameType that use this feature:
POPUPMENU, SLIDER, SIMPLESTATUSBAR, STATUSBAR

@patch 1.31
*/
native BlzFrameSetValue                            takes framehandle frame, real value returns nothing

/**
Gets the current Frame Value.

@async
@patch 1.31
*/
native BlzFrameGetValue                            takes framehandle frame returns real

/**
@patch 1.31
*/
native BlzFrameSetMinMaxValue                      takes framehandle frame, real minValue, real maxValue returns nothing

/**
SLIDER accuracy for User.
@patch 1.31
*/
native BlzFrameSetStepSize                         takes framehandle frame, real stepSize returns nothing

/**
@patch 1.31
*/
native BlzFrameSetSize                             takes framehandle frame, real width, real height returns nothing

/**
SimpleFrames only
@patch 1.31
@param color Four byte integer of the form 0xaarrggbb. You can also use
*/
native BlzFrameSetVertexColor                      takes framehandle frame, integer color returns nothing

/**
Used to reorder the children of a Frame.
SimpleFrames have fixed internal Layers. Which only contain String/Textures.
For SimpleFrames Level sets them higher/lower to all other SimpleFrames.

@patch 1.31
@param level bigger number gives a higher position.
*/
native BlzFrameSetLevel                            takes framehandle frame, integer level returns nothing

/**
@patch 1.31
*/
native BlzFrameSetParent                           takes framehandle frame, framehandle parent returns nothing

/**
@patch 1.31
*/
native BlzFrameGetParent                           takes framehandle frame returns framehandle

/**
@async
@patch 1.31
*/
native BlzFrameGetHeight                           takes framehandle frame returns real

/**
@async
@patch 1.31
*/
native BlzFrameGetWidth                            takes framehandle frame returns real

/**
Only works for String (SimpleFrames)
@patch 1.31
*/
native BlzFrameSetFont                             takes framehandle frame, string fileName, real height, integer flags returns nothing

/**
@patch 1.31
*/
native BlzFrameSetTextAlignment                    takes framehandle frame, textaligntype vert, textaligntype horz returns nothing



/**
The event starts for all players when one player triggers it.

The Event Getter functions. 

* `BlzGetTriggerFrame`
* `BlzGetTriggerFrameEvent`
* `BlzGetTriggerFrameValue`
* `BlzGetTriggerFrameText`
* `GetTriggerPlayer`

`BlzGetTriggerFrameValue` & `BlzGetTriggerFrameText` are only set when the
FrameEventEvent has use of them.

@patch 1.31
*/
native BlzTriggerRegisterFrameEvent                takes trigger whichTrigger, framehandle frame, frameeventtype eventId returns event

/**
@patch 1.31
*/
native BlzGetTriggerFrame                          takes nothing returns framehandle

/**
Ignores String/Texture

@patch 1.32.6
*/
native BlzFrameGetChildrenCount                    takes framehandle frame returns integer

/**
Valid Indexes are 0 to `BlzFrameGetChildrenCount` - 1.
Ignores String/Texture.
Breaks `BlzGetOriginFrame` when the same frame is first get using `BlzFrameGetChild`.

@patch 1.32.6
*/
native BlzFrameGetChild                            takes framehandle frame, integer index returns framehandle

/**
@patch 1.31
*/
native BlzGetTriggerFrameEvent                     takes nothing returns frameeventtype

/**
Returns the user input value of the triggered frame. (Slider, popupmenu, scrollbar...)
One has to use this native to sync user input, if that is needed.

@note This is a hidden native in PTR 1.31 (has to be declared to be usable in Jass).

@patch 1.31
*/
native BlzGetTriggerFrameValue                     takes nothing returns real

/**
Returns the user input text of the triggered frame. (EditBox)
One has to use this native to sync user input, if that is needed.

Limited to something like ~255 bytes.

@note This is a hidden native in PTR 1.31 (has to be declared to be usable in Jass).

@patch 1.31
*/
native BlzGetTriggerFrameText                      takes nothing returns string

/**
Sets cursor visibility

@param enable true to show, false to hide cursor

@patch 1.31
*/
native BlzEnableCursor                             takes boolean enable returns nothing

/**
x & y are px upto the used resolution `BlzGetLocalClientWidth()` `BlzGetLocalClientHeight()`

@patch 1.31
*/
native BlzSetMousePos                              takes integer x, integer y returns nothing

/**
Returns true if Warcraft 3 window is in focus.

@async
@patch 1.31
*/
native BlzIsLocalClientActive                      takes nothing returns boolean

/**
Returns the unit that is currently hovered by the mouse of the local player.
@async
@patch 1.31
*/
native BlzGetMouseFocusUnit                        takes nothing returns unit

/**
Uses a new Texture for the minimap.
@patch 1.31
*/
native BlzChangeMinimapTerrainTex                  takes string texFile returns boolean




/**
Create an event that listens to messages sent by player with prefix. (see: `BlzSendSyncData`)

One can create a player SyncEvent for any prefix with `TriggerRegisterPlayerEvent(whichTrigger, whichPlayer, EVENT_PLAYER_SYNC_DATA)`.

`GetTriggerPlayer()` is the message source.

@param fromServer "should be false"

@patch 1.31
*/
native BlzTriggerRegisterPlayerSyncEvent           takes trigger whichTrigger, player whichPlayer, string prefix, boolean fromServer returns event

/**
The player running this function sends a string message to all players.

@param prefix
identity
Limited to something like 255 bytes

@param data Limited to something like 255 bytes

@patch 1.31
*/
native BlzSendSyncData                             takes string prefix, string data returns boolean

/**
@patch 1.31
*/
native BlzGetTriggerSyncPrefix                     takes nothing returns string

/**
@patch 1.31
*/
native BlzGetTriggerSyncData                       takes nothing returns string



/**
@patch 1.31
@param metaKey
Bitfield. MetaKeys are "none"(0), "shift"(1), "control"(2), "alt"(4) and "META"(8) (windows key). They can be combined 2 + 4 = 6.
The player needs to holds this metakeys to trigger the event.
Inside the OnPress of a Metakey like OSKEY_LSHIFT the shift bit is set.

@param keyDown
(true) OnPress. In V1.31.1 This happens once, In V1.32.10 repeats until released
(false) OnRelease

*/
native BlzTriggerRegisterPlayerKeyEvent            takes trigger whichTrigger, player whichPlayer, oskeytype key, integer metaKey, boolean keyDown returns event

/**
@patch 1.31
*/
native BlzGetTriggerPlayerKey                      takes nothing returns oskeytype

/**
@patch 1.31
*/
native BlzGetTriggerPlayerMetaKey                  takes nothing returns integer

/**
@patch 1.31
*/
native BlzGetTriggerPlayerIsKeyDown                takes nothing returns boolean

/**
Gets the width (pixels) of the Warcraft 3 window.

@async
@patch 1.31
*/
native BlzGetLocalClientWidth                      takes nothing returns integer


/**
Gets the height (pixels) of the Warcraft 3 window.

@async
@patch 1.31
*/
native BlzGetLocalClientHeight                     takes nothing returns integer

/**
Returns the used warcraft 3 Lcid

    //  English (US)            = 'enUS' 
    //  English (UK)            = 'enGB' 
    //  French                  = 'frFR' 
    //  German                  = 'deDE' 
    //  Spanish                 = 'esES' 
    //  Italian                 = 'itIT' 
    //  Czech                   = 'csCZ' 
    //  Russian                 = 'ruRU' 
    //  Polish                  = 'plPL' 
    //  Portuguese (Brazilian)  = 'ptBR' 
    //  Portuguese (Portugal)   = 'ptPT' 
    //  Turkish                 = 'tkTK' 
    //  Japanese                = 'jaJA' 
    //  Korean                  = 'koKR' 
    //  Chinese (Traditional)   = 'zhTW' 
    //  Chinese (Simplified)    = 'zhCN' 
    //  Thai                    = 'thTH'
	
@note Warcraft 3 Lcids can be found in `config.ini` inside the CASC.

@async
@patch 1.31
*/
native BlzGetLocale                                takes nothing returns string

/**
@patch 1.31
*/
native BlzDisplayChatMessage                       takes player whichPlayer, integer recipient, string message returns nothing



/**
@patch 1.32
*/
native CreateCommandButtonEffect                   takes integer abilityId, string order returns commandbuttoneffect

/**
@patch 1.32
*/
native CreateUpgradeCommandButtonEffect            takes integer whichUprgade returns commandbuttoneffect

/**
@patch 1.32
*/
native CreateLearnCommandButtonEffect              takes integer abilityId returns commandbuttoneffect

/**
@patch 1.32
*/
native DestroyCommandButtonEffect                  takes commandbuttoneffect whichEffect returns nothing
