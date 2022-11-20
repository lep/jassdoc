// Multiboard API

// Create a multiboard object

/**
Creates a new multiboard and returns its handle.

The new multiboard by default:

- does not have a title
- row and column count are 0
- is not displayed
- is not minimized

To display a multiboard after creation, you must use `MultiboardDisplay`.

@note Multiboards must be destroyed to prevent leaks: `DestroyMultiboard`.

@note Only one multiboard can be visible at a time.
However there's a workaround using [Frame API](https://www.hiveworkshop.com/threads/ui-showing-3-multiboards.316610/).

@note There's a bug that causes big multiboards to
[freeze/crash the game on 1.33](https://www.hiveworkshop.com/threads/maximizing-the-multiboard-leads-to-freezing-game-with-the-latest-reforged-patch.341873/#post-3550996).

@bug Do not use this in a global initialisation as it crashes the game there.
*/
native CreateMultiboard                 takes nothing returns multiboard


/**
Destroys the multiboard and frees the handle.

@bug **Fixed in 1.33:** Crash on 1.30-1.32.10 (earlier?) when a multiboard is destroyed
while `ShowInterface` is false for a player and the game crashes later, once turned on.

`ShowInterface` is used by the cinematic mode, also known as "letterbox mode" as GUI trigger.

**Workaround:** hide the multiboard before destroying it, see: `MultiboardMinimize`.

**Bug reports:**
[Cinematic mode, multiboard](https://www.hiveworkshop.com/threads/fatal-error-after-cinematics.316707/post-3358087),
[toggling letterbox mode](https://www.hiveworkshop.com/threads/1-31-1-bug-destroymultiboard-causes-crash-after-disabling-letterbox.315554/),
[multiboard](https://www.hiveworkshop.com/threads/destroying-or-hiding-timer-window-causes-game-to-crash.310883/post-3312587).
*/
native DestroyMultiboard                takes multiboard lb returns nothing


/**
Shows or hides the multiboard.

Can be used to force a multiboard update.

@param lb Target multiboard
@param show `true` to show, `false` to hide.

@note Multiboards can not be shown at map-init. Use a wait or a zero-timer to
display as soon as possible.

@note See: `IsMultiboardDisplayed`.

@bug `MultiboardDisplay(mb,false)`, where mb is an arbitrary non-null multiboard
will close any open multiboard, regardless of whether it's `mb` or not.
<http://www.wc3c.net/showthread.php?p=971681#post971681>
*/
native MultiboardDisplay                takes multiboard lb, boolean show returns nothing

/**
Returns true if multiboard is visible, false if not shown.

@param lb Target multiboard.

@note See: `MultiboardDisplay`.
*/
native IsMultiboardDisplayed            takes multiboard lb returns boolean


/**
Minimizes/maximizes the multiboard. This is equivalent to clicking the small ↑ ↓ buttons in-game.

Can be used to force a multiboard update.

A maximized multiboard shows its contents and draws the content borders, even
if it has 0 rows and columns. When minimized only the title is shown.

@note See: `IsMultiboardMinimized`.
*/
native MultiboardMinimize               takes multiboard lb, boolean minimize returns nothing


/**
Returns true if minimized, false if maximized.

@async

@param lb Target multiboard.

@note See: `MultiboardMinimize`.
*/
native IsMultiboardMinimized            takes multiboard lb returns boolean


/**
Erases all items in a multiboard and sets row count to 0, column count to 0.
The multiboard's name is preserved.

@note *Implementation-specific:* Clearing a multiboard does not automatically invalidate
previous `multiboarditem` handles. If you expand the multiboard again, you'll be able to reuse
old handles. BUT you really shouldn't be doing this, it seems to be a buggy/undefined behavior.
When you clear or shrink a table, it's best to release old cell (item) handles with `MultiboardReleaseItem`.

@param lb Target multiboard.

@note See: `DestroyMultiboard` to remove, `MultiboardDisplay` to hide a multiboard.
*/
native MultiboardClear                  takes multiboard lb returns nothing


/**
Sets a multiboard's name.

The new text appears instantly.
The multiboard will expand as wide as necessary to display the title.

@note See: `MultiboardGetTitleText`

@param lb Target multiboard.
@param label New name.
*/
native MultiboardSetTitleText           takes multiboard lb, string label returns nothing


/**
Returns multiboard's name.

@note See: `MultiboardSetTitleText`.
*/
native MultiboardGetTitleText           takes multiboard lb returns string


/**
Sets the default color for multiboard name.

This is different than using color codes. If you use a color code in text,
it will override this color.

@note You can use this to avoid using color tags and text manipulation in code.

@param lb Target multiboard.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 transparency, please set to 255.
A value of 0 is complete transparency, while a value of 255 is complete opacity.

@note See: `MultiboardSetItemValueColor`.
*/
native MultiboardSetTitleTextColor      takes multiboard lb, integer red, integer green, integer blue, integer alpha returns nothing


/**
Returns the number of content rows (lines, horizontal) for the multiboard.

@note See: `MultiboardSetRowCount`, `MultiboardGetColumnCount`.

@param lb Target multiboard.
*/
native MultiboardGetRowCount            takes multiboard lb returns integer


/**
Returns the number of content columns (vertical) for the multiboard.

@note See: `MultiboardSetColumnCount`.

@param lb Target multiboard.
*/
native MultiboardGetColumnCount         takes multiboard lb returns integer


/**
Sets the number of content columns (vertical) for the multiboard.

@note See: `MultiboardGetColumnCount`.

@param lb Target multiboard.
*/
native MultiboardSetColumnCount         takes multiboard lb, integer count returns nothing


/**
Sets the number of content rows (lines, horizontal) for the multiboard.

@bug It is only safe to change the row count by one. Use multiple calls for bigger values.
<http://www.hiveworkshop.com/forums/l-715/m-250775/> (has test map)
<http://www.hiveworkshop.com/forums/t-269/w-234897/> (has only code)

@note See: `MultiboardGetRowCount`.

@param lb Target multiboard.
*/
native MultiboardSetRowCount            takes multiboard lb, integer count returns nothing



/**
Sets rendering properties for all cells.

@note See: `MultiboardSetItemStyle` for a detailed description.

@param lb Target multiboard.
*/
native MultiboardSetItemsStyle          takes multiboard lb, boolean showValues, boolean showIcons returns nothing


/**
Sets new text for all cells.

@note See: `MultiboardSetItemValue` for a detailed description.

@param lb Target multiboard.
*/
native MultiboardSetItemsValue          takes multiboard lb, string value returns nothing


/**
Sets the default color for text in all cell.

This is different than using color codes. If you use a color code in text,
it will override this color.

@note You can use this to avoid using color tags and text manipulation in code.

@param lb Target multiboard.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 alpha color, please set to 255.

@note See: `MultiboardSetItemValueColor`.
*/
native MultiboardSetItemsValueColor     takes multiboard lb, integer red, integer green, integer blue, integer alpha returns nothing


/**
Sets the new width for all cells.

@param lb Target multiboard.
@param width New cell width expressed as screen width. `1.0` = 100% of screen width,
`0.05` = 5% of screen width.

@note See: `MultiboardSetItemWidth` for a detailed description.
*/
native MultiboardSetItemsWidth          takes multiboard lb, real width returns nothing


/**
Sets a new icon for all cells.

@note See: `MultiboardSetItemIcon` for a detailed description.

@param lb Target multiboard.
@param iconPath Path to new icon texture.
*/
native MultiboardSetItemsIcon           takes multiboard lb, string iconPath returns nothing


// funcs for modifying individual items

/**
Acquires and returns a new handle for the multiboard cell.

@note Because a new handle is created each time, the handle must be
freed with `MultiboardReleaseItem`. The handle is different even if you
retrieve the same cell of the multiboard (v1.32.10, Lua).

@note The parameter order of `row` and `column` is (y,x) if you think of coordinates.

@param lb Target multiboard.
@param row In which row is the target cell (Y-coord, up-down). Starts from 0.
@param column in which column is the target cell (X-coord, left-right). Starts from 0.
*/
native MultiboardGetItem                takes multiboard lb, integer row, integer column returns multiboarditem


/**
Destroys the handle previously created with `MultiboardGetItem`.

It must be used to prevent leaks. Releasing the handle does not destroy or modify the
item.
*/
native MultiboardReleaseItem            takes multiboarditem mbi returns nothing


/**
Sets rendering properties of the multiboard cell.
Hiding the icon or text does not erase it.

There is no way to get a cell's style.

@param mbi Target cell handle.
@param showValue `true` to render text, `false` to hide text.
@param showIcon `true` to render icon, `false` to hide icon.

@note See: `MultiboardSetItemsStyle`.
*/
native MultiboardSetItemStyle           takes multiboarditem mbi, boolean showValue, boolean showIcon returns nothing


/**
Sets the cell's text. It is empty by default.

@note You must make sure the new text will fit in current width by setting
`MultiboardSetItemWidth` appropriately. If the width is too small, the text will be
cut off.

@param mbi Target cell handle.
@param val New text.

@note See: `MultiboardSetItemsValue`.
*/
native MultiboardSetItemValue           takes multiboarditem mbi, string val returns nothing


/**
Sets the default color for the cell text.

This is different than using color codes. If you use a color code in text,
it will override this color.

@note You can use this to avoid using color tags and text manipulation in code.

@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 alpha color, please set to 255.

@note See: `MultiboardSetItemsValueColor`.
*/
native MultiboardSetItemValueColor      takes multiboarditem mbi, integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets the new text width for the cell.

Default width is `0.03` (3%), this is enough to fit 1-3 characters
(depending on font and character).

@bug **NOTE!** Multiboard's total width is calculated based on ONLY the first row's
widths.

*Example:* Your first row is very short, but second row is twice is long.

*Result:* The second row will not fit inside the table and overflow to the right,
beyond the visible area.

**Summary:** To set the multiboard width, set the width of columns in the first row.

@bug Although the column width is set immediately and items in the same row are
moved left/right, the multiboard is not redrawn to accomodate the new width.

To update the entire multiboard's width, you must manually minimize/maximize
the multiboard or call `MultiboardDisplay(udg_myMultiboard, true)`
or `MultiboardMinimize(udg_myMultiboard, false)`.

For example, if you only change the width of cell at (x=0, y=0) to
be 0.2x of screen width, then the cell (x=1, y=0) will be moved right beyond the
visible screen.

@param mbi Target cell handle.
@param width New cell width expressed as screen width. `1.0` = 100% of screen width,
`0.05` = 5% of screen width.

The multiboard is right-aligned (begins at the right) at the window border.
See Tasyen's
[The Big UI-Frame Tutorial](https://www.hiveworkshop.com/pastebin/e23909d8468ff4942ccea268fbbcafd1.20598#PosFrames)
for the explanation of screen width.

@note See: `MultiboardSetItemsWidth`.
*/
native MultiboardSetItemWidth           takes multiboarditem mbi, real width returns nothing


/**
Sets the cell's icon. It is a grey eye icon by default.

@note Setting an invalid texture path will result in an undefined texture (100% green).

@param mbi Target cell handle.
@param iconFileName Path to new icon texture.

@note See: `MultiboardSetItemsIcon`.
*/
native MultiboardSetItemIcon            takes multiboarditem mbi, string iconFileName returns nothing


/**
While enabled, completely stops displaying any multiboards. It does not modify
any multiboards' display state. Useful for cinematics.

Once disabled, shows the last displayed (enabled) multiboard.

@param flag `true` to not render any multiboards, `false` to render multiboards.

@note See: `MultiboardDisplay` to modify an individual multiboard.
*/
native MultiboardSuppressDisplay        takes boolean flag returns nothing
