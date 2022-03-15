

/**
@patch 1.31
*/
native BlzEnableSelections                         takes boolean enableSelection, boolean enableSelectionCircle returns nothing

/**
@patch 1.31
*/
native BlzIsSelectionEnabled                       takes nothing returns boolean

/**
@patch 1.31
*/
native BlzIsSelectionCircleEnabled                 takes nothing returns boolean

/**
@patch 1.31
*/
native BlzCameraSetupApplyForceDurationSmooth      takes camerasetup whichSetup, boolean doPan, real forcedDuration, real easeInDuration, real easeOutDuration, real smoothFactor returns nothing

/**
@patch 1.31
*/
native BlzEnableTargetIndicator                    takes boolean enable returns nothing

/**
@patch 1.31
*/
native BlzIsTargetIndicatorEnabled                 takes nothing returns boolean


/**
@patch 1.31
@param index
to high values will return the frame from the last valid Index

The first time a Frame enters the map's script it takes a handleId.
*/
native BlzGetOriginFrame                           takes originframetype frameType, integer index returns framehandle

/**
@async
@patch 1.31
Enable/Disable autoreposing of some default frames, when the window state/size changes.
*/
native BlzEnableUIAutoPosition                     takes boolean enable returns nothing

/**
@async
@patch 1.31

Hides/Shows most of the default match UI. Unaffected are
Mouse, Command Buttons, Chat, Messages, TimerDialog, Multiboard, Leaderboard and ConsoleUIBackdrop

(De)Activades some autoreposing of default frames
*/
native BlzHideOriginFrames                         takes boolean enable returns nothing

/**
@patch 1.31

Loads in a TOCFile, to add/define Frame-Blueprints or Localized Strings
A TOC file contains a list, Each line is a path to a fdf (not case sensitve)
BUG: The TOC needs to end with one or two empty lines

*/
native BlzLoadTOCFile                              takes string TOCFile returns boolean

/**
@patch 1.31

Create a new Frame using a Frame-BluePrint name (fdf) as child of owner.
BluePrint needs to be loaded over TOC & fdf.
Owner and BluePrint have to be from the Frame family.
Can only create rootFrames.
Created Frames are stored into the game's Frame-Storage, BlzGetFrameByName(name, createContext). Overwrites occupied slots.
*/
native BlzCreateFrame                              takes string name, framehandle owner, integer priority, integer createContext returns framehandle

/**
@patch 1.31

Like BlzCreateFrame but for the SimpleFrame family, Frame "SIMPLExxxx".
*/
native BlzCreateSimpleFrame                        takes string name, framehandle owner, integer createContext returns framehandle

/**
@patch 1.31

Create & Define a new (Simple)Frame.
Can use a root-(Simple)Frame-BluePrint with inherits, when that is done it needs to be a loaded BluePrint.
*/
native BlzCreateFrameByType                        takes string typeName, string name, framehandle owner, string inherits, integer createContext returns framehandle

/**
@patch 1.31
*/
native BlzDestroyFrame                             takes framehandle frame returns nothing

/**
@async
@patch 1.31

Unbinds a point of FrameA and places it relative to a point of FrameB.
When FrameB moves FrameA's point will keep this rule and moves with it.

Each point of a frame can be placed to one point.
By placing multiple points of one Frame a Size is enforced.

*/
native BlzFrameSetPoint                            takes framehandle frame, framepointtype point, framehandle relative, framepointtype relativePoint, real x, real y returns nothing

/**
@async
@patch 1.31

unbinds & places point of frame to x/y.

Coords are for the 4:3 Screen
|0.0/0.8           0.8/0.6|
|                         |
|         0.4/0.3         |
|                         |
|0.0/0.0           0.8/0.0|
0.0/0.0 is bottomLeft (Minimap)
0.8/0.6 is TopRight (UpkeepCost
In widescreen format one can go further left with -x or further right with x > 0.8
Only some Frames and their Children/Offspring can leave 4:3.
SimpleFrames, Leaderboard, TimerDialog, Multiboard, ConsoleUIBackdrop
*/
native BlzFrameSetAbsPoint                         takes framehandle frame, framepointtype point, real x, real y returns nothing

/**
@async
@patch 1.31
Unbinds all points of frame.
Useful to move frames with the next SetPoint.
*/
native BlzFrameClearAllPoints                      takes framehandle frame returns nothing

/**
@async
@patch 1.31
@param frame 
the frame moved/resized
*/
native BlzFrameSetAllPoints                        takes framehandle frame, framehandle relative returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameSetVisible                          takes framehandle frame, boolean visible returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameIsVisible                           takes framehandle frame returns boolean


/**
@patch 1.31

Read from the internal Frame-Storage.
The first time a Frame enters the map's script it takes a handleId.
*/
native BlzGetFrameByName                           takes string name, integer createContext returns framehandle

/**
@patch 1.31

Inherited Frames lose their Name.
SimpleFrames return an empty String.
*/
native BlzFrameGetName                             takes framehandle frame returns string

/**
@async
@patch 1.31
Ignores visibility
Trigger FRAMEEVENT_CONTROL_CLICK
*/
native BlzFrameClick                               takes framehandle frame returns nothing

/**
@async
@patch 1.31

Supports Warcraft 3 TextCommands
Color Codes   |cffffcc00
Line seperator  |n \n
*/
native BlzFrameSetText                             takes framehandle frame, string text returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameGetText                             takes framehandle frame returns string

/**
@async
@patch 1.31
Start a NewLine and add text (TEXTAREA).
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
@patch 1.31

SimpleFrames only
@param color
-> BlzConvertColor
*/
native BlzFrameSetTextColor                        takes framehandle frame, integer color returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameSetFocus                            takes framehandle frame, boolean flag returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameSetModel                            takes framehandle frame, string modelFile, integer cameraIndex returns nothing

/**
@async
@patch 1.31
Turns on/off Interactivity/Events of frame
Can Swap Color/Texture.
(false) Removes KeyboardFocus.
The frame's Tooltip is still shown on hover.
*/
native BlzFrameSetEnable                           takes framehandle frame, boolean enabled returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameGetEnable                           takes framehandle frame returns boolean

/**
@async
@param alpha
0 to 255
@patch 1.31
Affects child-Frames, when they don't have an own Alpha.
*/
native BlzFrameSetAlpha                            takes framehandle frame, integer alpha returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameGetAlpha                            takes framehandle frame returns integer

/**
@async
@patch 1.31
*/
native BlzFrameSetSpriteAnimate                    takes framehandle frame, integer primaryProp, integer flags returns nothing

/**
@async
@patch 1.31

@param flag 
0 strechted, 1 tiles (BACKDROP)

@param blend
use transparency

Overwrittes some fdf setup.
*/
native BlzFrameSetTexture                          takes framehandle frame, string texFile, integer flag, boolean blend returns nothing

/**
@async
@patch 1.31
Affects child-Frames, when they don't have an own Scale.
*/
native BlzFrameSetScale                            takes framehandle frame, real scale returns nothing

/**
@patch 1.31
frame needs to be able to take mouse input.
tooltip is limited to 4:3, but not it's children.
SimpleFrame tooltips are not hidden with this call.
frame and tooltip have to be from the same Family (Frames/SimpleFrames).
tooltip can only serve one frame.
Undoing this is not possible.
Crashs the game, on hover, when done twice (same pair).
*/
native BlzFrameSetTooltip                          takes framehandle frame, framehandle tooltip returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameCageMouse                           takes framehandle frame, boolean enable returns nothing

/**
@async
@patch 1.31
Sets the current Value when the FrameType uses that feature:
POPUPMENU, SLIDER, SIMPLESTATUSBAR, STATUSBAR
*/
native BlzFrameSetValue                            takes framehandle frame, real value returns nothing

/**
@async
@patch 1.31
Gets the current local Value.
*/
native BlzFrameGetValue                            takes framehandle frame returns real

/**
@async
@patch 1.31
*/
native BlzFrameSetMinMaxValue                      takes framehandle frame, real minValue, real maxValue returns nothing

/**
@async
@patch 1.31
SLIDER accuracy for User.
*/
native BlzFrameSetStepSize                         takes framehandle frame, real stepSize returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameSetSize                             takes framehandle frame, real width, real height returns nothing

/**
@async
@patch 1.31
SimpleFrames only
@param color
-> BlzConvertColor
*/
native BlzFrameSetVertexColor                      takes framehandle frame, integer color returns nothing

/**
@patch 1.31
@param level
bigger number gives a higher position.
Used to reorder the children of a Frame.
SimpleFrames have fixed internal Layers. Which only contain String/Textures. For SimpleFrames Level sets them higher/lower to all other SimpleFrames.
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
@async
@patch 1.31
*/
native BlzFrameSetFont                             takes framehandle frame, string fileName, real height, integer flags returns nothing

/**
@async
@patch 1.31
*/
native BlzFrameSetTextAlignment                    takes framehandle frame, textaligntype vert, textaligntype horz returns nothing



/**
@patch 1.31
The event starts for all players when one player triggers it.

The Event Getter functions. 
BlzGetTriggerFrame      returns framehandle
BlzGetTriggerFrameEvent returns frameeventtype
BlzGetTriggerFrameValue returns real
BlzGetTriggerFrameText  returns string
GetTriggerPlayer        returns player

BlzGetTriggerFrameValue & BlzGetTriggerFrameText are only set when the FrameEventEvent has use of them.

*/
native BlzTriggerRegisterFrameEvent                takes trigger whichTrigger, framehandle frame, frameeventtype eventId returns event

/**
@patch 1.31
*/
native BlzGetTriggerFrame                          takes nothing returns framehandle

/**
@patch 1.32.6

Ignores String/Texture
*/
native BlzFrameGetChildrenCount                    takes framehandle frame returns integer

/**
@patch 1.32.6
valid Indexes are 0 to BlzFrameGetChildrenCount - 1.
Ignores String/Texture
Breaks BlzGetOriginFrame when the same frame is first get using BlzFrameGetChild.
*/
native BlzFrameGetChild                            takes framehandle frame, integer index returns framehandle

/**
@patch 1.31
*/
native BlzGetTriggerFrameEvent                     takes nothing returns frameeventtype

/**
@patch 1.31
*/
native BlzGetTriggerFrameValue                     takes nothing returns real

/**
@patch 1.31

Limited to something like ~255 bytes.
*/
native BlzGetTriggerFrameText                      takes nothing returns string

/**
@patch 1.31
*/
native BlzEnableCursor                             takes boolean enable returns nothing

/**
@async
@patch 1.31
x & y are px upto the used resolution BlzGetLocalClientWidth() BlzGetLocalClientHeight()
*/
native BlzSetMousePos                              takes integer x, integer y returns nothing

/**
@async
@patch 1.31
Has Warcraft 3 the focus?
*/
native BlzIsLocalClientActive                      takes nothing returns boolean

/**
@async
@patch 1.31
Returns the unit that is currently hovered by the mouse of the local player.
*/
native BlzGetMouseFocusUnit                        takes nothing returns unit

/**
@patch 1.31
Uses a new Texture for the minimap.
*/
native BlzChangeMinimapTerrainTex                  takes string texFile returns boolean




/**
@patch 1.31
Create an event that listens to messages send by player with prefix.

One can create a player SyncEvent for any prefix with TriggerRegisterPlayerEvent(whichTrigger, whichPlayer, EVENT_PLAYER_SYNC_DATA).

GetTriggerPlayer() is the message source.
*/
native BlzTriggerRegisterPlayerSyncEvent           takes trigger whichTrigger, player whichPlayer, string prefix, boolean fromServer returns event

/**
@async
@patch 1.31

The player running this function sends a string message to all players.

@param prefix
identity
Limited to something like 255 bytes

@param data
Limited to something like 255 bytes
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
@async
@patch 1.31
*/
native BlzGetLocalClientWidth                      takes nothing returns integer


/**
@async
@patch 1.31
*/
native BlzGetLocalClientHeight                     takes nothing returns integer

/**
@async
@patch 1.31
returns the used warcraft 3 Lcid
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
