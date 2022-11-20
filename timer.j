// Timer API

native CreateTimer          takes nothing returns timer

/**
@bug Destroying does not pause timer, so if call of its callback is scheduled,
then callback is called with `GetElapsedTimer` being `null`.
*/
native DestroyTimer         takes timer whichTimer returns nothing

/**
Starts a previously created timer that calls a function when timeout reaches 0.

It is affected by gamespeed at any point of it execution, if the gamespeed is changed at 50% of timeout duration, the rest of the timeout will be correctly affected by new gamespeed.

@param whichTimer Handle to timer.
@param timeout Delay in seconds.
@param periodic
True: repeat timer after expiration (loop).
False: timer only runs once.

@param handlerFunc Callback function to be executed when timer expires.

@note See: `GetExpiredTimer` to retrieve the handle of the expired timer inside handlerFunc.

@note The table below shows how often a 0 millisecond timer is executed
in comparison with `TriggerRegisterTimerEvent`
(aka `TriggerRegisterTimerEventPeriodic`).

| Trigger or Timer \ Tick count  |   ROC 1.0 | Reforged 1.32.10 |
|--------------------------------|----------:|-----------------:|
| 1000ms Trigger periodic        |      1 Hz |             1 Hz |
| 100ms Trigger periodic         |     10 Hz |            10 Hz |
| 20ms Trigger periodic          |     50 Hz |            50 Hz |
| 10ms Trigger periodic          |    100 Hz |           100 Hz |
| 5ms Trigger periodic           |    200 Hz |           100 Hz |
| 1ms Trigger periodic           |   1000 Hz |           100 Hz |
| 0ms Trigger periodic           |  10077 Hz |           100 Hz |
| 1ms Timer                      |   1000 Hz |          1000 Hz |
| 0ms Timer                      |  10077 Hz |         10077 Hz |
*/
native TimerStart           takes timer whichTimer, real timeout, boolean periodic, code handlerFunc returns nothing

/**
@note If passed timer is paused or has expired,
this function returns `(TimerGetTimeout - TimerGetRemaining)`.
@bug If passed timer was resumed by `ResumeTimer`,
this function returns amount of time elapsed after last resuming.
*/
native TimerGetElapsed      takes timer whichTimer returns real

/**
@note Returns remaining time of passed timer while timer is running or paused by `PauseTimer`.
@bug After non-periodic timer expires, this function returns remaining time that was at last pause of this timer.
*/
native TimerGetRemaining    takes timer whichTimer returns real

native TimerGetTimeout      takes timer whichTimer returns real

native PauseTimer           takes timer whichTimer returns nothing

/**
@note Has no effect if passed timer is running.
@bug If passed timer is paused or has expired, launches it for `TimerGetRemaining`,
and after this time is elapsed, launches it again for `TimerGetTimeout`.
After that passed timer is stopped even if it is periodic.
*/
native ResumeTimer          takes timer whichTimer returns nothing

/**
@bug Returns `null` if timer is destroyed right before callback call.
@bug Might crash the game if called when there is no expired timer.
<http://www.wc3c.net/showthread.php?t=84131>
*/
native GetExpiredTimer      takes nothing returns timer
