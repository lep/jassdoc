// Camera API

native SetCameraPosition            takes real x, real y returns nothing

native SetCameraQuickPosition       takes real x, real y returns nothing

native SetCameraBounds              takes real x1, real y1, real x2, real y2, real x3, real y3, real x4, real y4 returns nothing

native StopCamera                   takes nothing returns nothing

native ResetToGameCamera            takes real duration returns nothing

native PanCameraTo                  takes real x, real y returns nothing

native PanCameraToTimed             takes real x, real y, real duration returns nothing

native PanCameraToWithZ             takes real x, real y, real zOffsetDest returns nothing

native PanCameraToTimedWithZ        takes real x, real y, real zOffsetDest, real duration returns nothing

native SetCinematicCamera           takes string cameraModelFile returns nothing

native SetCameraRotateMode          takes real x, real y, real radiansToSweep, real duration returns nothing

native SetCameraField               takes camerafield whichField, real value, real duration returns nothing

/**
Changes one of the game camera's options whichField by offset over duration seconds.
*/
native AdjustCameraField            takes camerafield whichField, real offset, real duration returns nothing

native SetCameraTargetController    takes unit whichUnit, real xoffset, real yoffset, boolean inheritOrientation returns nothing

native SetCameraOrientController    takes unit whichUnit, real xoffset, real yoffset returns nothing



native CreateCameraSetup                    takes nothing returns camerasetup

native CameraSetupSetField                  takes camerasetup whichSetup, camerafield whichField, real value, real duration returns nothing

native CameraSetupGetField                  takes camerasetup whichSetup, camerafield whichField returns real

native CameraSetupSetDestPosition           takes camerasetup whichSetup, real x, real y, real duration returns nothing

native CameraSetupGetDestPositionLoc        takes camerasetup whichSetup returns location

native CameraSetupGetDestPositionX          takes camerasetup whichSetup returns real

native CameraSetupGetDestPositionY          takes camerasetup whichSetup returns real

native CameraSetupApply                     takes camerasetup whichSetup, boolean doPan, boolean panTimed returns nothing

native CameraSetupApplyWithZ                takes camerasetup whichSetup, real zDestOffset returns nothing

native CameraSetupApplyForceDuration        takes camerasetup whichSetup, boolean doPan, real forceDuration returns nothing

native CameraSetupApplyForceDurationWithZ   takes camerasetup whichSetup, real zDestOffset, real forceDuration returns nothing



native CameraSetTargetNoise             takes real mag, real velocity returns nothing

native CameraSetSourceNoise             takes real mag, real velocity returns nothing



native CameraSetTargetNoiseEx           takes real mag, real velocity, boolean vertOnly returns nothing

native CameraSetSourceNoiseEx           takes real mag, real velocity, boolean vertOnly returns nothing



native CameraSetSmoothingFactor         takes real factor returns nothing



native SetCineFilterTexture             takes string filename returns nothing

native SetCineFilterBlendMode           takes blendmode whichMode returns nothing

native SetCineFilterTexMapFlags         takes texmapflags whichFlags returns nothing

native SetCineFilterStartUV             takes real minu, real minv, real maxu, real maxv returns nothing

native SetCineFilterEndUV               takes real minu, real minv, real maxu, real maxv returns nothing

native SetCineFilterStartColor          takes integer red, integer green, integer blue, integer alpha returns nothing

native SetCineFilterEndColor            takes integer red, integer green, integer blue, integer alpha returns nothing

native SetCineFilterDuration            takes real duration returns nothing

native DisplayCineFilter                takes boolean flag returns nothing

native IsCineFilterDisplayed            takes nothing returns boolean



native SetCinematicScene                takes integer portraitUnitId, playercolor color, string speakerTitle, string text, real sceneDuration, real voiceoverDuration returns nothing

native EndCinematicScene                takes nothing returns nothing

native ForceCinematicSubtitles          takes boolean flag returns nothing



native GetCameraMargin                  takes integer whichMargin returns real


/**
Return-value for the local players camera only.
*/
constant native GetCameraBoundMinX          takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraBoundMinY          takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraBoundMaxX          takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraBoundMaxY          takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraField              takes camerafield whichField returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraTargetPositionX    takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraTargetPositionY    takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraTargetPositionZ    takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraTargetPositionLoc  takes nothing returns location

/**
Return-value for the local players camera only.
*/
constant native GetCameraEyePositionX       takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraEyePositionY       takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraEyePositionZ       takes nothing returns real

/**
Return-value for the local players camera only.
*/
constant native GetCameraEyePositionLoc     takes nothing returns location
