// Timer API

native CreateTimer          takes nothing returns timer

native DestroyTimer         takes timer whichTimer returns nothing

native TimerStart           takes timer whichTimer, real timeout, boolean periodic, code handlerFunc returns nothing

native TimerGetElapsed      takes timer whichTimer returns real

native TimerGetRemaining    takes timer whichTimer returns real

native TimerGetTimeout      takes timer whichTimer returns real

native PauseTimer           takes timer whichTimer returns nothing

native ResumeTimer          takes timer whichTimer returns nothing

native GetExpiredTimer      takes nothing returns timer
