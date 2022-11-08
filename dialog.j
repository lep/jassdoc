// Dialog API

/**
Creates a new dialog. It is empty and hidden by default.

Since this creates an object and returns a handle, it must be freed when no longer needed
with `DialogDestroy`.

An empty dialog must be populated with buttons
(`DialogSetMessage`, `DialogAddButton`, `DialogAddQuitButton`)
and finally displayed with `DialogDisplay`.

@note While a dialog is open, the player can only interact with the dialog.
Everything else is disabled.

An empty dialog completely blocks all player input (except multiboard interaction and
probably other scripted custom elements). A player can only exit the game with Alt+F4.

@bug The top-bar menu buttons are greyed out when a dialog is shown. If the player presses
Alt+F4 and then clicks "Cancel", the menu buttons become visible and clickable but do nothing.
*/
native DialogCreate                 takes nothing returns dialog


/**
Destroys the dialog and frees the handle.

Due to a bug, you must first hide the dialog for all players (who have it open).

@bug If you destroy a dialog that is still shown, it will no longer show **but**
the player who had it open will be unable to interact with the game. Everything will be still
disabled, all menus and units unclickable.
*/
native DialogDestroy                takes dialog whichDialog returns nothing


/**
Completely clears the dialog, its title and buttons, even if it is already open.

You must hide the dialog first, else the player will need to quit the game,
because they will be unable to click anything or send a chat message.

@param whichDialog Target dialog to clear.
*/
native DialogClear                  takes dialog whichDialog returns nothing


/**
Sets the menu title.

@note If the dialog is not set (empty string), then no vertical space is reserved for it.
The buttons start at the very top.

@note The new message shows up instantly, even when the menu is already open.
@note Unlike with buttons, if the message is too long it overflows to the left and right beyond the
screen edges (it is centered).

@param whichDialog Target dialog.
@param messageText New title.
*/
native DialogSetMessage             takes dialog whichDialog, string messageText returns nothing


/**
Creates a menu button and returns a handle to it.

You must save the button handle to later compare it to the selected button in
a `EVENT_DIALOG_BUTTON_CLICK` using `GetClickedButton` and `GetClickedDialog`.

New buttons are added to the bottom of the menu.

@note If the menu is already open, you must refresh the menu with `DialogDisplay`
to show new buttons.

@note **Line-width:** With the default font (v1.32.10) there's just enough space to display
`Yo dawg I put this text in here.` or 19 full-width characters like "@" (at character).
If longer, the text becomes multi-line, up to 3 lines max.
If longer than 3 full lines, the rest of string is not shown.

@note **Hotkey (uppercase):**
When adding a hotkey use the uppercase, e.g. `'F'` instead of `'f'` as it
does not work with lowercased keys. The button still gets triggered when the player
presses a lowercased letter.

@note **Duplicated hotkeys**: When multiple buttons have the same hotkey, the last button has priority.

@note **Hotkeys are layout-dependent.**

In other words, the English QWERTY, the German QWERTZ and the French AZERTY layouts etc. will have some
keyboard keys on different physical buttons, based on user's currently enabled layout.

The Russian keyboard layout adheres to QWERTY (as an example of a non-latin layout).

@note Hotkeys like "@" (ASCII 64) don't work (or maybe they have a different integer value).
On a QWERTY layout you need to press SHIFT+2 to enter "@".

@note You can add up to 12 working buttons.

@bug
The 13th button will still render correctly, but not work when clicked/hotkey is used.
The 14th button will render outside the dialog border background.
The 15th button will render outside the visible area (you'll see a few pixels of it at the bottom).

@param whichDialog Target dialog to add the button to.
@param buttonText Custom text.
@param hotkey Integer value of the ASCII upper-case character for the hotkey.
Example: "F" = 70.
*/
native DialogAddButton              takes dialog whichDialog, string buttonText, integer hotkey returns button


/**
Creates a menu button that will exit the game for the player who clicks it.
Returns a handle of button.

See the detailed description in `DialogAddButton`.

@param whichDialog Target dialog to add the button to.
@param doScoreScreen
When a button with `true` is pressed, you quit the map and see the end game leaderboards.

When a button with `false` is pressed, you quit the map to game's main menu.

@param buttonText Custom text.
@param hotkey Integer value of the ASCII upper-case character for the hotkey.
Example: "F" = 70.
*/
native DialogAddQuitButton          takes dialog whichDialog, boolean doScoreScreen, string buttonText, integer hotkey returns button


/**
Open/close the dialog for the target player.

Since the dialogs are created for all players, they are hidden by default. Then you display the
dialog to players who should see and interact with it.

@note Technically, because every player knows about each dialog,
cheaters could interact with dialogs that are invisible to them.

@note Dialogs can not be shown at map-init. Use a wait or a zero-timer to
display it as soon as possible.

@param whichPlayer Target player to whom to show the dialog.
@param whichDialog Target dialog.
@param flag `true` to show (or refresh), `false` to hide.
*/
native DialogDisplay                takes player whichPlayer, dialog whichDialog, boolean flag returns nothing
