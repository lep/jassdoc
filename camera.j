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


/**
Creates a new camerasetup object with the following default values.

|                   |               |
|-------------------|---------------|
|Target Coordinates |( 0.00 , 0.00 )|
|Z-Offset           | 0.00          |
|Rotation           | 90.00         |
|Angle of Attack    | 304.00        |
|Distance           | 1650.00       |
|Roll               | 0.00          |
|Field of View      | 70.00         |
|Far Clipping       | 5000.00       |

*/
native CreateCameraSetup                    takes nothing returns camerasetup

/**
Assigns a value to the specified field for a camerasetup. The input angles should be in degrees.

@param whichSetup
The camera setup.

@param whichField
The field of the camerasetup. 

@param value
The value to assign to the field.

@param duration
The duration over which the field will be set. If the duration is greater
than 0, the changes will be made gradually once the camera setup is applied.
*/
native CameraSetupSetField                  takes camerasetup whichSetup, camerafield whichField, real value, real duration returns nothing

/**
Returns the value of the specified field for a camerasetup. The angle of attack,
field of view, roll, and rotation are all returned in degrees, unlike `GetCameraField`.

@param whichSetup
The camera setup.

@param whichField
The field of the camerasetup.

@note The angle of attack, field of view, roll, and rotation are all returned in degrees.
*/
native CameraSetupGetField                  takes camerasetup whichSetup, camerafield whichField returns real

/**
Sets the target coordinates for a camerasetup over a duration. The coordinate
change will only be applied when `CameraSetupApply` (or some other variant) is ran.

@param whichSetup
The camera setup.

@param x
The target x-coordinate.

@param y
The target y-coordinate.

@param duration
The coordinates will be applied over this duration once the camera setup is applied.
*/
native CameraSetupSetDestPosition           takes camerasetup whichSetup, real x, real y, real duration returns nothing

/**
Returns the target location of a camerasetup.

@param whichSetup
The camera setup.
*/
native CameraSetupGetDestPositionLoc        takes camerasetup whichSetup returns location

/**
Returns the target x-coordinate of a camerasetup.

@param whichSetup
The camera setup.
*/
native CameraSetupGetDestPositionX          takes camerasetup whichSetup returns real

/**
Returns the target y-coordinate of a camerasetup.

@param whichSetup
The camera setup.
*/
native CameraSetupGetDestPositionY          takes camerasetup whichSetup returns real

/**
Applies the camerasetup, altering the current camera's fields to match those of the camera setup.

@param whichSetup
The camerasetup to apply.

@param doPan
If set to true, it will move the current camera's target coordinates to the
camera setup's target coordinates. If false, the camera will not move
coordinates, but will still apply the other fields.

@param panTimed
If set to true, then it will change the camera's properties over the times specified in CameraSetupSetField.

*/
native CameraSetupApply                     takes camerasetup whichSetup, boolean doPan, boolean panTimed returns nothing

/**
Applies the camerasetup with a custom z-offset, altering the current camera's
fields to match those of the camera setup. The z-offset input will override
the z-offset specified by the camerasetup through `CameraSetupSetField`.

@param whichSetup The camerasetup to apply.

@param zDestOffset
The camera's z-offset will gradually change to this value over the specified duration.

@bug If a player pauses the game after the camerasetup has been applied, the
z-offset of the game camera will change to the z-offset of the camerasetup for that player. 
*/
native CameraSetupApplyWithZ                takes camerasetup whichSetup, real zDestOffset returns nothing

/**
Applies the camerasetup over a certain duration, altering the current
camera's fields to match those of the camera setup.

@param whichSetup
The camerasetup to apply.

@param doPan
If set to true, it will move the current camera's target coordinates to the
camera setup's target coordinates. If false, the camera will not move
coordinates, but will still apply the other fields.

@param forceDuration
The duration it will take to apply all the camera fields.
It will ignore the times set by CameraSetupSetField.
*/
native CameraSetupApplyForceDuration        takes camerasetup whichSetup, boolean doPan, real forceDuration returns nothing

/**
Applies the camerasetup over a certain duration with a custom z-offset value,
altering the current camera's fields to match those of the camera setup.
The z-offset input will override the z-offset specified by `CameraSetupSetField`.

@param whichSetup
The camerasetup to apply.

@param zDestOffset
The camera's z-offset will gradually change to this value over the specified duration.

@param forceDuration
The duration it will take to apply all the camera fields.
It will ignore the times set by CameraSetupSetField.
*/
native CameraSetupApplyForceDurationWithZ   takes camerasetup whichSetup, real zDestOffset, real forceDuration returns nothing


/**
Causes the camera's target to sway(the camera's target, not the camera's perspective).
The higher the magnitude, the higher the range of swaying.
The higher the velocity, the more rapidly the swaying occurs.

@param mag
The magnitude of the swaying.

@param velocity
The speed of the swaying.

*/
native CameraSetTargetNoise             takes real mag, real velocity returns nothing

/**
Causes the camera's source to sway (the camera's perspective, not the camera's target).
The higher the magnitude, the higher the range of swaying.
The higher the velocity, the more rapidly the swaying occurs.
This will not affect the camera's target coordinates.

@param mag
The magnitude of the swaying.

@param velocity
The speed of the swaying.
*/
native CameraSetSourceNoise             takes real mag, real velocity returns nothing


/**
Causes the camera's target to sway, just like CameraSetTargetNoise. (the camera's target, not the camera's perspective) The higher the magnitude, the higher the range of swaying. The higher the velocity, the more rapidly the swaying occurs.

Causes the camera's source to sway (the camera's perspective, not the camera's target).
The higher the magnitude, the higher the range of swaying.
The higher the velocity, the more rapidly the swaying occurs.
This will not affect the camera's target coordinates.


@param mag
The magnitude of the swaying.

@param velocity
The speed of the swaying.

@param vertOnly
Stands for "vertical only". If set to true, then the swaying will only modify target distance and z-offset.

*/
native CameraSetTargetNoiseEx           takes real mag, real velocity, boolean vertOnly returns nothing

/**
Causes the camera to sway in the same fashion as `CameraSetSourceNoise`.

@param mag
The magnitude of the swaying.

@param velocity
The speed of the swaying.

@param vertOnly
Stands for "vertical only". If true, then only the angle of attack, target distance, and z-offset of the camera will be modified. (the rotation will not be modified)
*/
native CameraSetSourceNoiseEx           takes real mag, real velocity, boolean vertOnly returns nothing



/**
Sets the game camera's smoothing factor for scrolling with the mouse/keyboard. The default smoothing factor for the standard game camera is 0, where upon scrolling, the camera will immediately come to a stop. As the factor increases, the camera eases into a stop more and more gradually.

@param factor
The smoothing factor. It is 0 by default.
*/
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

@async
*/
constant native GetCameraBoundMinX          takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraBoundMinY          takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraBoundMaxX          takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraBoundMaxY          takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraField              takes camerafield whichField returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraTargetPositionX    takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraTargetPositionY    takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraTargetPositionZ    takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraTargetPositionLoc  takes nothing returns location

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraEyePositionX       takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraEyePositionY       takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraEyePositionZ       takes nothing returns real

/**
Return-value for the local players camera only.

@async
*/
constant native GetCameraEyePositionLoc     takes nothing returns location


/**
@patch 1.32
*/
native BlzCameraSetupSetLabel               takes camerasetup whichSetup, string label returns nothing

/**
@patch 1.32
*/
native BlzCameraSetupGetLabel               takes camerasetup whichSetup returns string


/**
@patch 1.32
*/
native CameraSetFocalDistance			takes real distance returns nothing

/**
@patch 1.32
*/
native CameraSetDepthOfFieldScale       takes real scale returns nothing


/**
@patch 1.32
*/
native SetCinematicAudio                takes boolean cinematicAudio returns nothing

/**
@patch 1.32
*/
native SetSoundFacialAnimationLabel takes sound soundHandle, string animationLabel returns boolean

/**
@patch 1.32
*/
native SetSoundFacialAnimationGroupLabel takes sound soundHandle, string groupLabel returns boolean

/**
@patch 1.32
*/
native SetSoundFacialAnimationSetFilepath takes sound soundHandle, string animationSetFilepath returns boolean

//Subtitle support that is attached to the soundHandle rather than as disperate data with the legacy UI

/**
@patch 1.32
*/
native SetDialogueSpeakerNameKey    takes sound soundHandle, string speakerName returns boolean

/**
@patch 1.32
*/
native GetDialogueSpeakerNameKey    takes sound soundHandle returns string

/**
@patch 1.32
*/
native SetDialogueTextKey           takes sound soundHandle, string dialogueText returns boolean

/**
@patch 1.32
*/
native GetDialogueTextKey           takes sound soundHandle returns string

