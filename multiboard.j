// Multiboard API

// Create a multiboard object

native CreateMultiboard                 takes nothing returns multiboard

native DestroyMultiboard                takes multiboard lb returns nothing


/**
@note Multiboars can not be shown at map-init. Use a wait or a zero-timer to
display as soon as possible.

@bug `MultiboardDisplay(mb,false)`, where mb is an arbitrary non-null multiboard
will close any open multiboard, regardless of whether it's `mb` or not.
<http://www.wc3c.net/showthread.php?p=971681#post971681>
*/
native MultiboardDisplay                takes multiboard lb, boolean show returns nothing

native IsMultiboardDisplayed            takes multiboard lb returns boolean



native MultiboardMinimize               takes multiboard lb, boolean minimize returns nothing

native IsMultiboardMinimized            takes multiboard lb returns boolean

native MultiboardClear                  takes multiboard lb returns nothing



native MultiboardSetTitleText           takes multiboard lb, string label returns nothing

native MultiboardGetTitleText           takes multiboard lb returns string

native MultiboardSetTitleTextColor      takes multiboard lb, integer red, integer green, integer blue, integer alpha returns nothing



native MultiboardGetRowCount            takes multiboard lb returns integer

native MultiboardGetColumnCount         takes multiboard lb returns integer



native MultiboardSetColumnCount         takes multiboard lb, integer count returns nothing


/**
@bug It is only safe to change the row count by one. Use multiple calls for bigger values.
<http://www.hiveworkshop.com/forums/l-715/m-250775/>
<http://www.hiveworkshop.com/forums/t-269/w-234897/>
*/
native MultiboardSetRowCount            takes multiboard lb, integer count returns nothing



/**
Applies the setting to all items.
*/
native MultiboardSetItemsStyle          takes multiboard lb, boolean showValues, boolean showIcons returns nothing

/**
Applies the setting to all items.
*/
native MultiboardSetItemsValue          takes multiboard lb, string value returns nothing

/**
Applies the setting to all items.
*/
native MultiboardSetItemsValueColor     takes multiboard lb, integer red, integer green, integer blue, integer alpha returns nothing

/**
Applies the setting to all items.
*/
native MultiboardSetItemsWidth          takes multiboard lb, real width returns nothing

/**
Applies the setting to all items.
*/
native MultiboardSetItemsIcon           takes multiboard lb, string iconPath returns nothing





// funcs for modifying individual items

native MultiboardGetItem                takes multiboard lb, integer row, integer column returns multiboarditem

native MultiboardReleaseItem            takes multiboarditem mbi returns nothing



native MultiboardSetItemStyle           takes multiboarditem mbi, boolean showValue, boolean showIcon returns nothing

native MultiboardSetItemValue           takes multiboarditem mbi, string val returns nothing

native MultiboardSetItemValueColor      takes multiboarditem mbi, integer red, integer green, integer blue, integer alpha returns nothing

native MultiboardSetItemWidth           takes multiboarditem mbi, real width returns nothing

native MultiboardSetItemIcon            takes multiboarditem mbi, string iconFileName returns nothing


/**
Meant to unequivocally suspend display of existing
and subsequently displayed multiboards.
*/
native MultiboardSuppressDisplay        takes boolean flag returns nothing
