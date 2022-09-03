// Native trigger interface


/**
Creates a new blank trigger object without any events, conditions or actions.
*/
native CreateTrigger    takes nothing returns trigger

/**
@bug Do not destroy the current running Trigger (when waits are involved) as
it can cause handle stack corruption as documented [here](http://www.wc3c.net/showthread.php?t=110519).
*/
native DestroyTrigger   takes trigger whichTrigger returns nothing

/**
Resets the evaluate and execution count of the given trigger back to zero.
*/
native ResetTrigger     takes trigger whichTrigger returns nothing

/**
See `DisableTrigger`. `EnableTrigger` enables the given trigger again, so it will be fired when the events registered on it occur.
*/
native EnableTrigger    takes trigger whichTrigger returns nothing

/**
Disables the given trigger. A disabled trigger is not fired by the events registered on it but `TriggerEvaluate` and `TriggerExecute` can still be used.
This can be reversed with `EnableTrigger`.
*/
native DisableTrigger   takes trigger whichTrigger returns nothing

/**
Tells whether the given trigger is enabled. See `EnableTrigger` and `DisableTrigger`. A trigger is enabled on default.
*/
native IsTriggerEnabled takes trigger whichTrigger returns boolean


/**
Marks the given trigger to wait/no longer wait for `TriggerSleepAction`s in sub trigger executions started via `TriggerExecuteWait`.
Since this is an attribute of the execution rather than the trigger object, this affects future runs of the given trigger, and not
those already started.
*/
native TriggerWaitOnSleeps   takes trigger whichTrigger, boolean flag returns nothing

/**
Tells whether the given trigger waits for `TriggerSleepAction`s in sub trigger executions started via `TriggerExecuteWait`.
See `TriggerWaitOnSleeps`.
*/
native IsTriggerWaitOnSleeps takes trigger whichTrigger returns boolean


/**
This returns the current unit in calls to the `GroupEnumUnits-`natives.
*/
constant native GetFilterUnit       takes nothing returns unit

/**
This returns the current unit in calls to the `ForGroup` native
*/
constant native GetEnumUnit         takes nothing returns unit



constant native GetFilterDestructable   takes nothing returns destructable

constant native GetEnumDestructable     takes nothing returns destructable



constant native GetFilterItem           takes nothing returns item

constant native GetEnumItem             takes nothing returns item



constant native GetFilterPlayer     takes nothing returns player

constant native GetEnumPlayer       takes nothing returns player



constant native GetTriggeringTrigger    takes nothing returns trigger

constant native GetTriggerEventId       takes nothing returns eventid

constant native GetTriggerEvalCount     takes trigger whichTrigger returns integer

constant native GetTriggerExecCount     takes trigger whichTrigger returns integer


/**
Tries to find a function with the given name and calls it in a new thread.

@note If this is called in a trigger action context, `ExecuteFunc` will use that trigger, so `GetTriggeringTrigger` will return it. If `ExecuteFunc` is
called in another type of context, it will spawn a new trigger, which can be seen with `GetTriggeringTrigger`.

@bug `ExecuteFunc` does not seem to release the trigger it spawns.

@note As `ExecuteFunc` will run the target function in a trigger action context one way or another, `TriggerSleepAction` can be used.
*/
native ExecuteFunc          takes string funcName returns nothing

native TriggerAddCondition    takes trigger whichTrigger, boolexpr condition returns triggercondition

native TriggerRemoveCondition takes trigger whichTrigger, triggercondition whichCondition returns nothing

native TriggerClearConditions takes trigger whichTrigger returns nothing



/**
Adds an action to be called when the given trigger is fired through registered events or through `TriggerExecute`.

@note More than one action can be added to the trigger. The actions run in the order they were added.

@note The same function can be used more than once on the same trigger.

@note Actions wait for their forerunner to finish. So if there are `TriggerSleepAction`s, subsequent actions will be delayed accordingly.

@note If an action execution crashes, subsequent actions will be unaffected and still be called.

@bug If an action execution crashes after a `TriggerSleepAction` in the same action execution, subsequent actions will not be run.

@note New actions added to the trigger during the execution of the actions won't be subject for execution for this run.
*/
native TriggerAddAction     takes trigger whichTrigger, code actionFunc returns triggeraction

/**
Removes an action from a trigger.

@bug If the actions of the trigger are currently running and the removed action was still pending to be called, it will still be called unless
there is a `TriggerSleepAction` after the `TriggerRemoveAction`.
*/
native TriggerRemoveAction  takes trigger whichTrigger, triggeraction whichAction returns nothing

/**
Removes all actions from a trigger.

@bug If the actions of the trigger are currently running, hereby removed actions still pending to be called will still be called. In contrast to
`TriggerRemoveAction`, this will be the case regardless if there is a `TriggerSleepAction` after `TriggerClearActions` or not.
*/
native TriggerClearActions  takes trigger whichTrigger returns nothing

/**
Makes a trigger execution sleep for a given duration. The thread will yield so other threads can do their work.

@note This works only in a trigger action execution context, not in trigger conditions nor for example in timer functions or `ForGroup` functions. However, it
also works in `ExecuteFunc` contexts, even if the `ExecuteFunc` call is not in a trigger action execution context.

@note This has many implications, see other trigger-related natives.

@note This ticks while the game was paused with the `PauseGame` native, unlike timers.

@note This does not tick while the game was paused by the user, neither in singleplayer nor in multiplayer. (But the Trigger Editor of the World Editor
denotes it as a real-time wait. Is this a bug?)
*/
native TriggerSleepAction   takes real timeout returns nothing

native TriggerWaitForSound  takes sound s, real offset returns nothing

/**
Evaluates all functions that were added to the trigger via `TriggerAddCondition`.
All return-values from all added condition-functions are `and`ed together as the final return-value.
Returns the boolean value of the return value from the condition-function.
So if the condition-functions return `0`/`0.0`/`null`, then `TriggerEvaluate`
will return `false`. Note that an empty string `""` would return `true`.

@note If a condition-function crashes the thread or does not return any value
`TriggerEvaluate` will return `false`.

@note If you want to return false for a condition-function that returns
string (for whatever reason) return `null` instead of `""`

@note *All* functions added via `TriggerAddCondition` are run.
There is no short-circuting. If you want short-circuting use `And` or `Or`.

@note All functions added via `TriggerAddCondition` are run in the order they
were added.
*/
native TriggerEvaluate      takes trigger whichTrigger returns boolean

/**
Calls the actions of a trigger in a new execution context. Control will return to the caller when the
trigger has finished or has been suspended via `TriggerSleepAction`.
*/
native TriggerExecute       takes trigger whichTrigger returns nothing

/**
Does the same as `TriggerExecute` but if the caller has been marked with `TriggerWaitOnSleeps` before its
execution, it will additionally wait for `TriggerSleepAction`s of the callee, so this really ensures that
the callee has finished. If there was a `TriggerSleepAction`, there will be a short delay before returning.
*/
native TriggerExecuteWait   takes trigger whichTrigger returns nothing

native TriggerSyncStart     takes nothing returns nothing

native TriggerSyncReady     takes nothing returns nothing
