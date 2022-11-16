// Timer Dialog API

/**
Creates a new timer dialog based on the underlying timer.
It is hidden by default and has "Remaining" as title (localized).

Timer dialog works as a visible countdown timer in the format: "Title hh:mm:ss".

Since this creates an object and returns a handle, it must be freed when no longer needed
with `DestroyTimerDialog`.

@note (v1.32.10, Lua) If `t` is nil then the dialog is still created,
but will never show any time.

Alternatively, you can set the visible time with `TimerDialogSetRealTimeRemaining`.

@param t connect the timer dialog to this timer, it'll always follow its
"time remaining".
*/
native CreateTimerDialog                takes timer t returns timerdialog


/**
Destroys the timer dialog and frees the handle.

This does not affect the timer you might have provided in `CreateTimerDialog`.

@param whichDialog target dialog
*/
native DestroyTimerDialog               takes timerdialog whichDialog returns nothing


/**
Sets the shown dialog title. Replaces the default "Remaining" text.

@note Depending on font and version, there's enough space to display
14 full-width characters like "@" (at character). If the text is wider,
it is shortened and an ellipsis "..." is shown at the end.

@note See: `TimerDialogSetTitle`, `TimerDialogSetTitleColor`, `TimerDialogSetTimeColor`

@param whichDialog target dialog
@param title new title
*/
native TimerDialogSetTitle              takes timerdialog whichDialog, string title returns nothing

/**
Sets the timer-dialogs color.

See: `TimerDialogSetTitle`, `TimerDialogSetTimeColor`

@param whichDialog The timerdialog
@param red 0-255 red color (value mod 256)
@param green 0-255 green color (value mod 256)
@param blue 0-255 blue color (value mod 256)
@param alpha (unused) 0-255 transparency, please set to 255.
A value of 0 is complete transparency, while a value of 255 is complete opacity.
*/
native TimerDialogSetTitleColor         takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets the timer-dialogs time color.

@note See: `TimerDialogSetTitleColor`

@param whichDialog The timerdialog
@param red 0-255 red color (value mod 256)
@param green 0-255 green color (value mod 256)
@param blue 0-255 blue color (value mod 256)
@param alpha (unused) 0-255 transparency, please set to 255.
A value of 0 is complete transparency, while a value of 255 is complete opacity.
*/
native TimerDialogSetTimeColor          takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing


/**
Set a new multiplier for the shown time remaining. Default is `1.0`.

The multiplier factor is applied literally to the displayed time:
`timerTimeRemainingSec * speedMultFactor`.

@note It does not affect the underlying timer `t` from `CreateTimerDialog`.
If you set the speed too high, the display will not become smoother as
it updates roughly 2-3 times per second.

@param whichDialog target dialog to modify the speed of
@param speedMultFactor new multiplicator factor

For factor `2.0` the displayed time will appear twice as fast (200% speed).

For factor `0.5` the displayed time will appear half as fast (50% speed).

Factor `0.0` will always display `00:00:00`.
*/
native TimerDialogSetSpeed              takes timerdialog whichDialog, real speedMultFactor returns nothing


/**
Show/hide the dialog for all players.

A timer dialog is displayed above a multiboard in the top-right corner.

@note Multiple timer dialogues stack from right to left, for example:
"Timer dialog 2  12:34:56" "Timer dialog 1  02:10:42".

@note If the timer has not been started yet, it will not show any time:
"Remaining ".

@note A dialog display can be toggled per-player by using it inside a
`GetLocalPlayer` condition.

@bug (v1.32.10) The second timerdialog's width and position is calculated and
displayed incorrectly in ultra-wide mode (beyond 1800x920, 1.95 ratio).

@bug (v1.32.10) If you toggle visibility of one dialog but not the other
in a single frame, the first dialog will appear below the second one.

```{.lua}
	tdialog = CreateTimerDialog(CreateTimer())
	TimerDialogSetTitle(tdialog, "Timer1 Dialog __ 1")
	TimerDialogDisplay(tdialog, true)
	tdialog2 = CreateTimerDialog(CreateTimer())
	TimerDialogSetTitle(tdialog2, "Timer2 Dialog")
	TimerDialogDisplay(tdialog2, true)
	-- Correct up to this point.
	-- This is buggy:
	TimerDialogDisplay(tdialog, false)
	TimerDialogDisplay(tdialog2, true)
	TimerDialogDisplay(tdialog, true)
	-- Now tdialog will appear beneath tdialog2.
```

**Workarounds:**

1. Hide *every* dialog, then show those that you need.
2. Introduce a sleep-wait before turning dialog display on.

@note See: `IsTimerDialogDisplayed`

@param whichDialog target dialog
@param display `true` to show, `false` to hide
*/
native TimerDialogDisplay               takes timerdialog whichDialog, boolean display returns nothing


/**
Returns `true` if the dialog is shown, `false` if it is hidden.

@note See: `TimerDialogDisplay`

@param whichDialog check visibility of this timer dialog
*/
native IsTimerDialogDisplayed           takes timerdialog whichDialog returns boolean

/**
Sets the timer dialog countdown to specified time and decouples it from the
provided timer in `CreateTimerDialog`.

@note For example if the dialog was created with a periodic timer, it would
reset the countdown like the timer when it reaches zero.

Once you set a custom time with this function, it will no longer follow the
timer. Once it reaches zero, it'll stay at zero.

@note There's no way to retrieve the internal timer value or to have an event
trigger.

@param whichDialog target dialog
@param timeRemaining new time in seconds
*/
native TimerDialogSetRealTimeRemaining  takes timerdialog whichDialog, real timeRemaining returns nothing
