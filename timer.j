// Timer API

native CreateTimer          takes nothing returns timer

/**
@bug Destroying does not pause timer,
so if call of its callback is scheduled,
then callback is called with GetElapsedTimer being null.
*/
native DestroyTimer         takes timer whichTimer returns nothing

native TimerStart           takes timer whichTimer, real timeout, boolean periodic, code handlerFunc returns nothing

/**
@note If passed timer is paused or has expired,
this function returns (TimerGetTimeout - TimerGetRemaining).
@bug If passed timer was resumed by ResumeTimer,
this function returns amount of time elapsed after last resuming.
*/
native TimerGetElapsed      takes timer whichTimer returns real

/**
@note Returns remaining time of passed timer while timer is running or paused by PauseTimer.
@bug After non-periodic timer expires, this function returns remaining time that was at last pause of this timer.
*/
native TimerGetRemaining    takes timer whichTimer returns real

native TimerGetTimeout      takes timer whichTimer returns real

native PauseTimer           takes timer whichTimer returns nothing

/**
@note Has no effect if passed timer is running.
@bug If passed timer is paused or has expired, launches it for TimerGetRemaining,
and after this time is elapsed, launches it again for TimerGetTimeout.
After that passed timer is stopped even if it is periodic.
*/
native ResumeTimer          takes timer whichTimer returns nothing

/**
@bug Might crash the game if called when there is no expired timer.
<http://www.wc3c.net/showthread.php?t=84131>
*/
native GetExpiredTimer      takes nothing returns timer
