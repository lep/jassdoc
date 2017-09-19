// Timer Dialog API

native CreateTimerDialog                takes timer t returns timerdialog

native DestroyTimerDialog               takes timerdialog whichDialog returns nothing

native TimerDialogSetTitle              takes timerdialog whichDialog, string title returns nothing

/**
Sets the timer-dialogs color.

@param whichDialog The timerdialog
@param red An integer from 0-255 determining the amount of red color.
@param green An integer from 0-255 determining the amount of green color.
@param blue An integer from 0-255 determining the amount of blue color.
@param alpha An integer from 0-255 determining the transparency. A value of 0 is complete transparency while a value of 255 is complete opacity.
*/
native TimerDialogSetTitleColor         takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets the timer-dialogs time color.

@param whichDialog The timerdialog
@param red An integer from 0-255 determining the amount of red color.
@param green An integer from 0-255 determining the amount of green color.
@param blue An integer from 0-255 determining the amount of blue color.
@param alpha An integer from 0-255 determining the transparency. A value of 0 is complete transparency while a value of 255 is complete opacity.
*/
native TimerDialogSetTimeColor          takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing

native TimerDialogSetSpeed              takes timerdialog whichDialog, real speedMultFactor returns nothing

native TimerDialogDisplay               takes timerdialog whichDialog, boolean display returns nothing

native IsTimerDialogDisplayed           takes timerdialog whichDialog returns boolean

native TimerDialogSetRealTimeRemaining  takes timerdialog whichDialog, real timeRemaining returns nothing
