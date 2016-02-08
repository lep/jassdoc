// Dialog API

native DialogCreate                 takes nothing returns dialog

native DialogDestroy                takes dialog whichDialog returns nothing

native DialogClear                  takes dialog whichDialog returns nothing

native DialogSetMessage             takes dialog whichDialog, string messageText returns nothing

native DialogAddButton              takes dialog whichDialog, string buttonText, integer hotkey returns button

native DialogAddQuitButton          takes dialog whichDialog, boolean doScoreScreen, string buttonText, integer hotkey returns button

native DialogDisplay                takes player whichPlayer, dialog whichDialog, boolean flag returns nothing