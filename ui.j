

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
*/
native BlzGetOriginFrame                           takes originframetype frameType, integer index returns framehandle

/**
@patch 1.31
*/
native BlzEnableUIAutoPosition                     takes boolean enable returns nothing

/**
@patch 1.31
*/
native BlzHideOriginFrames                         takes boolean enable returns nothing

/**
@patch 1.31
*/
native BlzLoadTOCFile                              takes string TOCFile returns boolean

/**
@patch 1.31
*/
native BlzCreateFrame                              takes string name, framehandle owner, integer priority, integer createContext returns framehandle

/**
@patch 1.31
*/
native BlzCreateSimpleFrame                        takes string name, framehandle owner, integer createContext returns framehandle

/**
@patch 1.31
*/
native BlzCreateFrameByType                        takes string typeName, string name, framehandle owner, string inherits, integer createContext returns framehandle

/**
@patch 1.31
*/
native BlzDestroyFrame                             takes framehandle frame returns nothing

/**
@patch 1.31
*/
native BlzFrameSetPoint                            takes framehandle frame, framepointtype point, framehandle relative, framepointtype relativePoint, real x, real y returns nothing

/**
@patch 1.31
*/
native BlzFrameSetAbsPoint                         takes framehandle frame, framepointtype point, real x, real y returns nothing

/**
@patch 1.31
*/
native BlzFrameClearAllPoints                      takes framehandle frame returns nothing

/**
@patch 1.31
*/
native BlzFrameSetAllPoints                        takes framehandle frame, framehandle relative returns nothing

/**
@patch 1.31
*/
native BlzFrameSetVisible                          takes framehandle frame, boolean visible returns nothing

/**
@patch 1.31
*/
native BlzFrameIsVisible                           takes framehandle frame returns boolean


/**
@patch 1.31
*/
native BlzGetFrameByName                           takes string name, integer createContext returns framehandle

/**
@patch 1.31
*/
native BlzFrameGetName                             takes framehandle frame returns string

/**
@patch 1.31
*/
native BlzFrameClick                               takes framehandle frame returns nothing

/**
@patch 1.31
*/
native BlzFrameSetText                             takes framehandle frame, string text returns nothing

/**
@patch 1.31
*/
native BlzFrameGetText                             takes framehandle frame returns string

/**
@patch 1.32.3
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
@patch 1.31
*/
native BlzFrameSetEnable                           takes framehandle frame, boolean enabled returns nothing

/**
@patch 1.31
*/
native BlzFrameGetEnable                           takes framehandle frame returns boolean

/**
@patch 1.31
*/
native BlzFrameSetAlpha                            takes framehandle frame, integer alpha returns nothing

/**
@patch 1.31
*/
native BlzFrameGetAlpha                            takes framehandle frame returns integer

/**
@patch 1.31
*/
native BlzFrameSetSpriteAnimate                    takes framehandle frame, integer primaryProp, integer flags returns nothing

/**
@patch 1.31
*/
native BlzFrameSetTexture                          takes framehandle frame, string texFile, integer flag, boolean blend returns nothing

/**
@patch 1.31
*/
native BlzFrameSetScale                            takes framehandle frame, real scale returns nothing

/**
@patch 1.31
*/
native BlzFrameSetTooltip                          takes framehandle frame, framehandle tooltip returns nothing

/**
@patch 1.31
*/
native BlzFrameCageMouse                           takes framehandle frame, boolean enable returns nothing

/**
@patch 1.31
*/
native BlzFrameSetValue                            takes framehandle frame, real value returns nothing

/**
@patch 1.31
*/
native BlzFrameGetValue                            takes framehandle frame returns real

/**
@patch 1.31
*/
native BlzFrameSetMinMaxValue                      takes framehandle frame, real minValue, real maxValue returns nothing

/**
@patch 1.31
*/
native BlzFrameSetStepSize                         takes framehandle frame, real stepSize returns nothing

/**
@patch 1.31
*/
native BlzFrameSetSize                             takes framehandle frame, real width, real height returns nothing

/**
@patch 1.31
*/
native BlzFrameSetVertexColor                      takes framehandle frame, integer color returns nothing

/**
@patch 1.31
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
@patch 1.31
*/
native BlzFrameGetHeight                           takes framehandle frame returns real

/**
@patch 1.31
*/
native BlzFrameGetWidth                            takes framehandle frame returns real

/**
@patch 1.31
*/
native BlzFrameSetFont                             takes framehandle frame, string fileName, real height, integer flags returns nothing

/**
@patch 1.31
*/
native BlzFrameSetTextAlignment                    takes framehandle frame, textaligntype vert, textaligntype horz returns nothing



/**
@patch 1.31
*/
native BlzTriggerRegisterFrameEvent                takes trigger whichTrigger, framehandle frame, frameeventtype eventId returns event

/**
@patch 1.31
*/
native BlzGetTriggerFrame                          takes nothing returns framehandle

/**
@patch 1.32.6
*/
native BlzFrameGetChildrenCount                    takes framehandle frame returns integer

/**
@patch 1.32.6
*/
native BlzFrameGetChild                            takes framehandle frame, integer index returns framehandle

/**
@patch 1.31
*/
native BlzGetTriggerFrameEvent                     takes nothing returns frameeventtype

/**
@patch 1.32.3
*/
native BlzGetTriggerFrameValue                     takes nothing returns real

/**
@patch 1.32.3
*/
native BlzGetTriggerFrameText                      takes nothing returns string

/**
@patch 1.31
*/
native BlzEnableCursor                             takes boolean enable returns nothing

/**
@patch 1.31
*/
native BlzSetMousePos                              takes integer x, integer y returns nothing

/**
@async
@patch 1.31
*/
native BlzIsLocalClientActive                      takes nothing returns boolean

/**
@async
@patch 1.31
*/
native BlzGetMouseFocusUnit                        takes nothing returns unit

/**
@patch 1.31
*/
native BlzChangeMinimapTerrainTex                  takes string texFile returns boolean




/**
@patch 1.31
*/
native BlzTriggerRegisterPlayerSyncEvent           takes trigger whichTrigger, player whichPlayer, string prefix, boolean fromServer returns event

/**
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
