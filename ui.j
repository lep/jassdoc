
native BlzEnableSelections                         takes boolean enableSelection, boolean enableSelectionCircle returns nothing
native BlzIsSelectionEnabled                       takes nothing returns boolean
native BlzIsSelectionCircleEnabled                 takes nothing returns boolean
native BlzCameraSetupApplyForceDurationSmooth      takes camerasetup whichSetup, boolean doPan, real forcedDuration, real easeInDuration, real easeOutDuration, real smoothFactor returns nothing
native BlzEnableTargetIndicator                    takes boolean enable returns nothing
native BlzIsTargetIndicatorEnabled                 takes nothing returns boolean

native BlzGetOriginFrame                           takes originframetype frameType, integer index returns framehandle
native BlzEnableUIAutoPosition                     takes boolean enable returns nothing
native BlzHideOriginFrames                         takes boolean enable returns nothing
native BlzLoadTOCFile                              takes string TOCFile returns boolean
native BlzCreateFrame                              takes string name, framehandle owner, integer priority, integer createContext returns framehandle
native BlzCreateSimpleFrame                        takes string name, framehandle owner, integer createContext returns framehandle
native BlzCreateFrameByType                        takes string typeName, string name, framehandle owner, string inherits, integer createContext returns framehandle
native BlzDestroyFrame                             takes framehandle frame returns nothing
native BlzFrameSetPoint                            takes framehandle frame, framepointtype point, framehandle relative, framepointtype relativePoint, real x, real y returns nothing
native BlzFrameSetAbsPoint                         takes framehandle frame, framepointtype point, real x, real y returns nothing
native BlzFrameClearAllPoints                      takes framehandle frame returns nothing
native BlzFrameSetAllPoints                        takes framehandle frame, framehandle relative returns nothing
native BlzFrameSetVisible                          takes framehandle frame, boolean visible returns nothing
native BlzFrameIsVisible                           takes framehandle frame returns boolean
native BlzGetFrameByName                           takes string name, integer createContext returns framehandle
native BlzFrameGetName                             takes framehandle frame returns string
native BlzFrameClick                               takes framehandle frame returns nothing
native BlzFrameSetText                             takes framehandle frame, string text returns nothing
native BlzFrameGetText                             takes framehandle frame returns string
native BlzFrameSetTextSizeLimit                    takes framehandle frame, integer size returns nothing
native BlzFrameGetTextSizeLimit                    takes framehandle frame returns integer
native BlzFrameSetTextColor                        takes framehandle frame, integer color returns nothing
native BlzFrameSetFocus                            takes framehandle frame, boolean flag returns nothing
native BlzFrameSetModel                            takes framehandle frame, string modelFile, integer cameraIndex returns nothing
native BlzFrameSetEnable                           takes framehandle frame, boolean enabled returns nothing
native BlzFrameGetEnable                           takes framehandle frame returns boolean
native BlzFrameSetAlpha                            takes framehandle frame, integer alpha returns nothing
native BlzFrameGetAlpha                            takes framehandle frame returns integer
native BlzFrameSetSpriteAnimate                    takes framehandle frame, integer primaryProp, integer flags returns nothing
native BlzFrameSetTexture                          takes framehandle frame, string texFile, integer flag, boolean blend returns nothing
native BlzFrameSetScale                            takes framehandle frame, real scale returns nothing
native BlzFrameSetTooltip                          takes framehandle frame, framehandle tooltip returns nothing
native BlzFrameCageMouse                           takes framehandle frame, boolean enable returns nothing
native BlzFrameSetValue                            takes framehandle frame, real value returns nothing
native BlzFrameGetValue                            takes framehandle frame returns real
native BlzFrameSetMinMaxValue                      takes framehandle frame, real minValue, real maxValue returns nothing
native BlzFrameSetStepSize                         takes framehandle frame, real stepSize returns nothing
native BlzFrameSetSize                             takes framehandle frame, real width, real height returns nothing
native BlzFrameSetVertexColor                      takes framehandle frame, integer color returns nothing
native BlzFrameSetLevel                            takes framehandle frame, integer level returns nothing
native BlzFrameSetParent                           takes framehandle frame, framehandle parent returns nothing
native BlzFrameGetParent                           takes framehandle frame returns framehandle
native BlzFrameGetHeight                           takes framehandle frame returns real
native BlzFrameGetWidth                            takes framehandle frame returns real
native BlzFrameSetFont                             takes framehandle frame, string fileName, real height, integer flags returns nothing
native BlzFrameSetTextAlignment                    takes framehandle frame, textaligntype vert, textaligntype horz returns nothing


native BlzTriggerRegisterFrameEvent                takes trigger whichTrigger, framehandle frame, frameeventtype eventId returns event
native BlzGetTriggerFrame                          takes nothing returns framehandle
native BlzGetTriggerFrameEvent                     takes nothing returns frameeventtype

native BlzEnableCursor                             takes boolean enable returns nothing
native BlzSetMousePos                              takes integer x, integer y returns nothing

/**
@async
*/
native BlzIsLocalClientActive                      takes nothing returns boolean

/**
@async
*/
native BlzGetMouseFocusUnit                        takes nothing returns unit
native BlzChangeMinimapTerrainTex                  takes string texFile returns boolean



native BlzTriggerRegisterPlayerSyncEvent           takes trigger whichTrigger, player whichPlayer, string prefix, boolean fromServer returns event
native BlzSendSyncData                             takes string prefix, string data returns boolean
native BlzGetTriggerSyncPrefix                     takes nothing returns string
native BlzGetTriggerSyncData                       takes nothing returns string


native BlzTriggerRegisterPlayerKeyEvent            takes trigger whichTrigger, player whichPlayer, oskeytype key, integer metaKey, boolean keyDown returns event
native BlzGetTriggerPlayerKey                      takes nothing returns oskeytype
native BlzGetTriggerPlayerMetaKey                  takes nothing returns integer
native BlzGetTriggerPlayerIsKeyDown                takes nothing returns boolean

/**
@async
*/
native BlzGetLocalClientWidth                      takes nothing returns integer


/**
@async
*/
native BlzGetLocalClientHeight                     takes nothing returns integer

/**
@async
*/
native BlzGetLocale                                takes nothing returns string
native BlzDisplayChatMessage                       takes player whichPlayer, integer recipient, string message returns nothing


