// Sound API

native NewSoundEnvironment          takes string environmentName returns nothing



native CreateSound                  takes string fileName, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate, string eaxSetting returns sound

native CreateSoundFilenameWithLabel takes string fileName, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate, string SLKEntryName returns sound

native CreateSoundFromLabel         takes string soundLabel, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate returns sound

native CreateMIDISound              takes string soundLabel, integer fadeInRate, integer fadeOutRate returns sound



native SetSoundParamsFromLabel      takes sound soundHandle, string soundLabel returns nothing

native SetSoundDistanceCutoff       takes sound soundHandle, real cutoff returns nothing

native SetSoundChannel              takes sound soundHandle, integer channel returns nothing

native SetSoundVolume               takes sound soundHandle, integer volume returns nothing

native SetSoundPitch                takes sound soundHandle, real pitch returns nothing

/**
@note Must be called immediately after calling StartSound.
*/
native SetSoundPlayPosition         takes sound soundHandle, integer millisecs returns nothing


/**
@note This call is only valid if the sound was created with 3d enabled
*/
native SetSoundDistances            takes sound soundHandle, real minDist, real maxDist returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled
*/
native SetSoundConeAngles           takes sound soundHandle, real inside, real outside, integer outsideVolume returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled
*/
native SetSoundConeOrientation      takes sound soundHandle, real x, real y, real z returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled
*/
native SetSoundPosition             takes sound soundHandle, real x, real y, real z returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled
*/
native SetSoundVelocity             takes sound soundHandle, real x, real y, real z returns nothing

/**
Attaches the sound soundHandle to unit whichUnit. Attaching sound to unit means that more far player stays from the unit to which the sound is attached, less loud the sound plays (the volume of the attached sound decreases with increasing distance).
@note This call is only valid if the sound was created with 3d enabled
*/
native AttachSoundToUnit            takes sound soundHandle, unit whichUnit returns nothing



native StartSound                   takes sound soundHandle returns nothing

native StopSound                    takes sound soundHandle, boolean killWhenDone, boolean fadeOut returns nothing

native KillSoundWhenDone            takes sound soundHandle returns nothing



// Music Interface. Note that if music is disabled, these calls do nothing

native SetMapMusic                  takes string musicName, boolean random, integer index returns nothing

native ClearMapMusic                takes nothing returns nothing



native PlayMusic                    takes string musicName returns nothing

native PlayMusicEx                  takes string musicName, integer frommsecs, integer fadeinmsecs returns nothing

native StopMusic                    takes boolean fadeOut returns nothing

native ResumeMusic                  takes nothing returns nothing



native PlayThematicMusic            takes string musicFileName returns nothing

native PlayThematicMusicEx          takes string musicFileName, integer frommsecs returns nothing

native EndThematicMusic             takes nothing returns nothing



native SetMusicVolume               takes integer volume returns nothing

native SetMusicPlayPosition         takes integer millisecs returns nothing

native SetThematicMusicPlayPosition takes integer millisecs returns nothing



// other music and sound calls

native SetSoundDuration             takes sound soundHandle, integer duration returns nothing

native GetSoundDuration             takes sound soundHandle returns integer

native GetSoundFileDuration         takes string musicFileName returns integer



native VolumeGroupSetVolume         takes volumegroup vgroup, real scale returns nothing

native VolumeGroupReset             takes nothing returns nothing



native GetSoundIsPlaying            takes sound soundHandle returns boolean

native GetSoundIsLoading            takes sound soundHandle returns boolean



native RegisterStackedSound         takes sound soundHandle, boolean byPosition, real rectwidth, real rectheight returns nothing

native UnregisterStackedSound       takes sound soundHandle, boolean byPosition, real rectwidth, real rectheight returns nothing