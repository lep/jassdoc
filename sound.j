// Sound API

native NewSoundEnvironment          takes string environmentName returns nothing


/**
Creates a sound handle.

@note You can only play the same sound handle once.

@note You can only play the same sound filepath four times.

@note Sounds of the same filepath (on different sound handles) must have a delay
of at least 0.1 seconds inbetween them to be played.
You can overcome this by starting one earlier and then using `SetSoundPosition`.

@note You can only play 16 sounds in general.

@param fileName The path to the file.

@param looping Looping sounds will restart once the sound duration has finished.

@param is3D 3D Sounds can be played on particular areas of the map. They are at
their loudest when the camera is close to the sound's coordinates.

@param fadeInRate How quickly the sound fades in. The higher the number, the
faster the sound fades in. Maximum number is 127.

@param fadeOutRate How quickly the sound fades out. The higher the number, the
faster the sound fades out. Maximum number is 127.

@param eaxSetting EAX is an acronym for environmental audio extensions. In the
sound editor, this corresponds to the "Effect" setting.
The known settings available in Warcraft III are:

| Value              |  Setting               |
|--------------------| ---------------------- |
|`"CombatSoundsEAX"` | combat                 |
|`"KotoDrumsEAX"`    | drums                  | 
|`"SpellsEAX"`       | spells                 |
|`"MissilesEAX"`     | missiles               |
|`"HeroAcksEAX"`     | hero acknowledgements  |
|`"DoodadsEAX"`      | doodads                |
|`"DefaultEAXON"`    | default                |

*/
native CreateSound                  takes string fileName, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate, string eaxSetting returns sound

/**
Creates a sound but applies default settings to the sound, which are found
under the label from the following SLK-files:

* UI\SoundInfo\AbilitySounds.slk
* UI\SoundInfo\AmbienceSounds.slk
* UI\SoundInfo\AnimSounds.slk
* UI\SoundInfo\DialogSounds.slk
* UI\SoundInfo\UISounds.slk
* UI\SoundInfo\UnitAckSounds.slk
* UI\SoundInfo\UnitCombatSounds.slk

@note You can only play the same sound handle once.

@note You can only play the same sound filepath four times.

@note Sounds of the same filepath (on different sound handles) must have a delay
of at least 0.1 seconds inbetween them to be played.
You can overcome this by starting one earlier and then using `SetSoundPosition`.

@note You can only play 16 sounds in general.

@param fileName The path to the file.

@param looping Looping sounds will restart once the sound duration has finished.

@param is3D 3D Sounds can be played on particular areas of the map. They are at
their loudest when the camera is close to the sound's coordinates.

@param fadeInRate How quickly the sound fades in. The higher the number,
the faster the sound fades in. Maximum number is 127.

@param fadeOutRate How quickly the sound fades out. The higher the number,
the faster the sound fades out. Maximum number is 127.

@param SLKEntryName the label out of one of the SLK-files, whose settings should be
used, e.g. values like volume, pitch, pitch variance, priority, channel, min distance, max distance, distance cutoff or eax.

*/
native CreateSoundFilenameWithLabel takes string fileName, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate, string SLKEntryName returns sound

native CreateSoundFromLabel         takes string soundLabel, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate returns sound

native CreateMIDISound              takes string soundLabel, integer fadeInRate, integer fadeOutRate returns sound



/**
Applies default settings to the sound, which are found under the label from the following SLK-files:

* UI\SoundInfo\AbilitySounds.slk
* UI\SoundInfo\AmbienceSounds.slk
* UI\SoundInfo\AnimSounds.slk
* UI\SoundInfo\DialogSounds.slk
* UI\SoundInfo\UISounds.slk
* UI\SoundInfo\UnitAckSounds.slk
* UI\SoundInfo\UnitCombatSounds.slk

@param soundHandle The sound to configure.
@param soundLabel the label out of one of the SLK-files, whose settings should be
used, e.g. values like volume, pitch, pitch variance, priority, channel, min distance, max distance, distance cutoff or eax.
*/
native SetSoundParamsFromLabel      takes sound soundHandle, string soundLabel returns nothing

native SetSoundDistanceCutoff       takes sound soundHandle, real cutoff returns nothing

native SetSoundChannel              takes sound soundHandle, integer channel returns nothing

/**
Sets the sounds volume.

@param soundHandle which sound.

@param volume Volume, between 0 and 127.
*/
native SetSoundVolume               takes sound soundHandle, integer volume returns nothing

/**
Tones the pitch of the sound, default value is 1. Increasing it you get the chipmunk
version and the sound becomes shorter, when decremented the sound becomes low-pitched and longer.

@bug This native has very weird behaviour.
See [this](http://www.hiveworkshop.com/threads/setsoundpitch-weirdness.215743/#post-2145419) for an explanation
and [this](http://www.hiveworkshop.com/threads/snippet-rapidsound.258991/#post-2611724) for a non-bugged implementation.
*/
native SetSoundPitch                takes sound soundHandle, real pitch returns nothing

/**
@note Must be called immediately after calling `StartSound`.
*/
native SetSoundPlayPosition         takes sound soundHandle, integer millisecs returns nothing


/**
@note This call is only valid if the sound was created with 3d enabled.
*/
native SetSoundDistances            takes sound soundHandle, real minDist, real maxDist returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled.
*/
native SetSoundConeAngles           takes sound soundHandle, real inside, real outside, integer outsideVolume returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled.
*/
native SetSoundConeOrientation      takes sound soundHandle, real x, real y, real z returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled.
*/
native SetSoundPosition             takes sound soundHandle, real x, real y, real z returns nothing

/**
@note This call is only valid if the sound was created with 3d enabled.
*/
native SetSoundVelocity             takes sound soundHandle, real x, real y, real z returns nothing

/**
Attaches the sound soundHandle to unit whichUnit. Attaching sound to unit means
that the more far away the player stays from the unit to which the sound is attached, the less
loud the sound plays (the volume of the attached sound decreases with increasing distance).

@note This call is only valid if the sound was created with 3d enabled.

@param soundHandle The 3D sound to play.
@param whichUnit The unit to attach the sound to.
*/
native AttachSoundToUnit            takes sound soundHandle, unit whichUnit returns nothing


/**
Starts the sound.

@note You can only play the same sound handle once.

@note You can only play 16 sounds in general.

@note Sounds of the same filepath (on different sound handles) must have a delay
of at least 0.1 seconds inbetween them to be played.
You can overcome this by starting one earlier and then using `SetSoundPosition`.
*/
native StartSound                   takes sound soundHandle returns nothing

/**
Starts playing a sound. 

@note An officially exported native in: 1.33.0 (checked v1.33.0.18897 PTR).
Unofficially available in: 1.32 (not declared a native, but visible in Lua).

@note The only difference to StartSound is the optional fadeIn (boolean).
@patch 1.33
*/
native StartSoundEx                 takes sound soundHandle, boolean fadeIn returns nothing

/**
Stops the sound.

@param soundHandle The sound to stop.
@param killWhenDone The sound gets destroyed if true.
@param fadeOut turns down the volume with `fadeOutRate` as stated in constructor.
*/
native StopSound                    takes sound soundHandle, boolean killWhenDone, boolean fadeOut returns nothing

/**
Destroys the handle when the sound has finished playing.
*/
native KillSoundWhenDone            takes sound soundHandle returns nothing



// Music Interface. Note that if music is disabled, these calls do nothing

/**
@note If music is disabled, these calls do nothing.
*/
native SetMapMusic                  takes string musicName, boolean random, integer index returns nothing

/**
Clears the map music applied via `SetMapMusic`.
*/
native ClearMapMusic                takes nothing returns nothing


/**
Sets the file as the current music for the map, and plays it.

@note Music is on its own channel and can be toggled on and off within the Warcraft III game menu.
@bug This native may cause a short lag spike as soon as the music starts. To circumvent this lag, stop the current music without fadeout before calling this function (`call StopMusic(false)`). 
@param musicName The path to the music file.
@note Should work with mp3s, midis and wavs.
*/
native PlayMusic                    takes string musicName returns nothing

/**
Sets the file as the current music for the map, and plays it.

@note Music is on its own channel and can be toggled on and off within the Warcraft III game menu.
@bug This native may cause a short lag spike as soon as the music starts. To circumvent this lag, stop the current music without fadeout before calling this function (`call StopMusic(false)`). 
@param musicName The path to the music file.
@param frommsecs At what offset the music starts. In milliseconds.
@param fadeinmsecs How long the music is faded in. In milliseconds.
@note Should work with mp3s, midis and wavs.
*/
native PlayMusicEx                  takes string musicName, integer frommsecs, integer fadeinmsecs returns nothing

/**
Stops the current music.
*/
native StopMusic                    takes boolean fadeOut returns nothing

/**
Resumes music.
*/
native ResumeMusic                  takes nothing returns nothing


/**
The thematic music does not play repeatedly, but interrupts the PlayMusic-music.

@note Only one thematic music at a time, cancels the previous one.
@note Probably meant for boss fights and similar where the sound should go in foreground.

@param musicFileName The path to the music file.
*/
native PlayThematicMusic            takes string musicFileName returns nothing

/**
The thematic music does not play repeatedly, but interrupts the PlayMusic-music.

@note Only one thematic music at a time, cancels the previous one.
@note Probably meant for boss fights and similar where the sound should go in foreground.

@param musicFileName The path to the music file.
@param frommsecs At what offset the music starts. In milliseconds.
*/
native PlayThematicMusicEx          takes string musicFileName, integer frommsecs returns nothing

/**
Stops thematic music.
*/
native EndThematicMusic             takes nothing returns nothing



/**
Sets the music volume.

@param volume Volume between 0 and 127.
*/
native SetMusicVolume               takes integer volume returns nothing

native SetMusicPlayPosition         takes integer millisecs returns nothing

native SetThematicMusicPlayPosition takes integer millisecs returns nothing



// other music and sound calls

native SetSoundDuration             takes sound soundHandle, integer duration returns nothing

/**
Returns sound length in milliseconds.

@note Beweare that this might return different values for different players
if you use native wc3-sounds as these can have different length in different languages.
This can cause desyncs if you use the duration for non-local stuff.

@async
*/
native GetSoundDuration             takes sound soundHandle returns integer

/**
Returns length of the sound file under the path in milliseconds.

@note Beweare that this might return different values for different players
if you use native wc3-sounds as these can have different length in different languages.
This can cause desyncs if you use the duration for non-local stuff.

@async
*/
native GetSoundFileDuration         takes string musicFileName returns integer



native VolumeGroupSetVolume         takes volumegroup vgroup, real scale returns nothing

native VolumeGroupReset             takes nothing returns nothing


/**
@note If you just started the sound this still returns false.

@async
*/
native GetSoundIsPlaying            takes sound soundHandle returns boolean

native GetSoundIsLoading            takes sound soundHandle returns boolean



native RegisterStackedSound         takes sound soundHandle, boolean byPosition, real rectwidth, real rectheight returns nothing

native UnregisterStackedSound       takes sound soundHandle, boolean byPosition, real rectwidth, real rectheight returns nothing

/**
@patch 1.32.2
*/
native SetThematicMusicVolume       takes integer volume returns nothing
