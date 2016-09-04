// Dialog API

native DialogCreate                 takes nothing returns dialog

native DialogDestroy                takes dialog whichDialog returns nothing

native DialogClear                  takes dialog whichDialog returns nothing

native DialogSetMessage             takes dialog whichDialog, string messageText returns nothing

/**
@note When adding a hotkey use the uppercase, e.g. `'F'` instead of `'f'` as it
does not work with lowercased keys. The button still gets triggered when the player
presses a lowercased letter.
*/
native DialogAddButton              takes dialog whichDialog, string buttonText, integer hotkey returns button

native DialogAddQuitButton          takes dialog whichDialog, boolean doScoreScreen, string buttonText, integer hotkey returns button

/**
@note Dialogs can not be shown at map-init. Use a wait or a zero-timer to
display as soon as possible.
*/
native DialogDisplay                takes player whichPlayer, dialog whichDialog, boolean flag returns nothing
