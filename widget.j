// Widget API

native GetWidgetLife takes widget whichWidget returns real

native SetWidgetLife takes widget whichWidget, real newLife returns nothing

/**
Returns X map coordinate of widget on success. Returns 0.0 if widget was removed or is null.

Some other types extend from widget, you can use widget-related functions on these too.

@note See: `GetWidgetY`
*/
native GetWidgetX takes widget whichWidget returns real

/**
Returns Y map coordinate of widget on success. Returns 0.0 if widget was removed or is null.

Some other types extend from widget, you can use widget-related functions on these too.

@note See: `GetWidgetX`
*/
native GetWidgetY takes widget whichWidget returns real

constant native GetTriggerWidget takes nothing returns widget
