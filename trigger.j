// Native trigger interface


native CreateTrigger    takes nothing returns trigger

native DestroyTrigger   takes trigger whichTrigger returns nothing

native ResetTrigger     takes trigger whichTrigger returns nothing

native EnableTrigger    takes trigger whichTrigger returns nothing

native DisableTrigger   takes trigger whichTrigger returns nothing

native IsTriggerEnabled takes trigger whichTrigger returns boolean



native TriggerWaitOnSleeps   takes trigger whichTrigger, boolean flag returns nothing

native IsTriggerWaitOnSleeps takes trigger whichTrigger returns boolean



constant native GetFilterUnit       takes nothing returns unit

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



native ExecuteFunc          takes string funcName returns nothing

native TriggerAddCondition    takes trigger whichTrigger, boolexpr condition returns triggercondition

native TriggerRemoveCondition takes trigger whichTrigger, triggercondition whichCondition returns nothing

native TriggerClearConditions takes trigger whichTrigger returns nothing



native TriggerAddAction     takes trigger whichTrigger, code actionFunc returns triggeraction

native TriggerRemoveAction  takes trigger whichTrigger, triggeraction whichAction returns nothing

native TriggerClearActions  takes trigger whichTrigger returns nothing

native TriggerSleepAction   takes real timeout returns nothing

native TriggerWaitForSound  takes sound s, real offset returns nothing

/**
Evaluates all functions that were added to the trigger via `TriggerAddCondition`.
All return-values from all added condition-functions are `and`ed together as the final return-value.
Returns the boolean value of the return value from the condition-function.
So if 0/0.0/null would be returned in the condition-function, `TriggerEvaluate`
would return false. Note that `""` would return `true`.

@note If a condition-function crashes the thread or does not return any value
`TriggerEvaluate` will return false.

@note If you want to return false for a condition-function that returns
string (for whatever reason) return `null` instead of `""`

@note *All* functions added via `TriggerAddCondition` are run.
There is no short-circuting. If you want short-circuting use `And` or `Or`.

@note All functions added via `TriggerAddCondition` are run in the order they
were added.
*/
native TriggerEvaluate      takes trigger whichTrigger returns boolean

native TriggerExecute       takes trigger whichTrigger returns nothing

native TriggerExecuteWait   takes trigger whichTrigger returns nothing

native TriggerSyncStart     takes nothing returns nothing

native TriggerSyncReady     takes nothing returns nothing
