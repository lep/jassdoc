// Timer API

native CreateTimer          takes nothing returns timer

native DestroyTimer         takes timer whichTimer returns nothing

native TimerStart           takes timer whichTimer, real timeout, boolean periodic, code handlerFunc returns nothing

native TimerGetElapsed      takes timer whichTimer returns real

native TimerGetRemaining    takes timer whichTimer returns real

native TimerGetTimeout      takes timer whichTimer returns real

native PauseTimer           takes timer whichTimer returns nothing

native ResumeTimer          takes timer whichTimer returns nothing

/**
@bug Might crash the game if called when there is no expired timer.
<http://www.wc3c.net/showthread.php?t=84131>
*/
native GetExpiredTimer      takes nothing returns timer
