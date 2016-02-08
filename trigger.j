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

native TriggerEvaluate      takes trigger whichTrigger returns boolean

native TriggerExecute       takes trigger whichTrigger returns nothing

native TriggerExecuteWait   takes trigger whichTrigger returns nothing

native TriggerSyncStart     takes nothing returns nothing

native TriggerSyncReady     takes nothing returns nothing