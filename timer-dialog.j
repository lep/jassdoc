// Timer Dialog API

native CreateTimerDialog                takes timer t returns timerdialog

native DestroyTimerDialog               takes timerdialog whichDialog returns nothing

native TimerDialogSetTitle              takes timerdialog whichDialog, string title returns nothing

native TimerDialogSetTitleColor         takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing

native TimerDialogSetTimeColor          takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing

native TimerDialogSetSpeed              takes timerdialog whichDialog, real speedMultFactor returns nothing

native TimerDialogDisplay               takes timerdialog whichDialog, boolean display returns nothing

native IsTimerDialogDisplayed           takes timerdialog whichDialog returns boolean

native TimerDialogSetRealTimeRemaining  takes timerdialog whichDialog, real timeRemaining returns nothing