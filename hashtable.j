
/**
@note You cannot create more than 255 hashtables
*/
native  InitHashtable    takes nothing returns hashtable



native  SaveInteger						takes hashtable table, integer parentKey, integer childKey, integer value returns nothing

native  SaveReal						takes hashtable table, integer parentKey, integer childKey, real value returns nothing

native  SaveBoolean						takes hashtable table, integer parentKey, integer childKey, boolean value returns nothing

native  SaveStr							takes hashtable table, integer parentKey, integer childKey, string value returns boolean

native  SavePlayerHandle				takes hashtable table, integer parentKey, integer childKey, player whichPlayer returns boolean

native  SaveWidgetHandle				takes hashtable table, integer parentKey, integer childKey, widget whichWidget returns boolean

native  SaveDestructableHandle			takes hashtable table, integer parentKey, integer childKey, destructable whichDestructable returns boolean

native  SaveItemHandle					takes hashtable table, integer parentKey, integer childKey, item whichItem returns boolean

native  SaveUnitHandle					takes hashtable table, integer parentKey, integer childKey, unit whichUnit returns boolean

native  SaveAbilityHandle				takes hashtable table, integer parentKey, integer childKey, ability whichAbility returns boolean

native  SaveTimerHandle					takes hashtable table, integer parentKey, integer childKey, timer whichTimer returns boolean

native  SaveTriggerHandle				takes hashtable table, integer parentKey, integer childKey, trigger whichTrigger returns boolean

native  SaveTriggerConditionHandle		takes hashtable table, integer parentKey, integer childKey, triggercondition whichTriggercondition returns boolean

native  SaveTriggerActionHandle			takes hashtable table, integer parentKey, integer childKey, triggeraction whichTriggeraction returns boolean

native  SaveTriggerEventHandle			takes hashtable table, integer parentKey, integer childKey, event whichEvent returns boolean

native  SaveForceHandle					takes hashtable table, integer parentKey, integer childKey, force whichForce returns boolean

native  SaveGroupHandle					takes hashtable table, integer parentKey, integer childKey, group whichGroup returns boolean

native  SaveLocationHandle				takes hashtable table, integer parentKey, integer childKey, location whichLocation returns boolean

native  SaveRectHandle					takes hashtable table, integer parentKey, integer childKey, rect whichRect returns boolean

native  SaveBooleanExprHandle			takes hashtable table, integer parentKey, integer childKey, boolexpr whichBoolexpr returns boolean

native  SaveSoundHandle					takes hashtable table, integer parentKey, integer childKey, sound whichSound returns boolean

native  SaveEffectHandle				takes hashtable table, integer parentKey, integer childKey, effect whichEffect returns boolean

native  SaveUnitPoolHandle				takes hashtable table, integer parentKey, integer childKey, unitpool whichUnitpool returns boolean

native  SaveItemPoolHandle				takes hashtable table, integer parentKey, integer childKey, itempool whichItempool returns boolean

native  SaveQuestHandle					takes hashtable table, integer parentKey, integer childKey, quest whichQuest returns boolean

native  SaveQuestItemHandle				takes hashtable table, integer parentKey, integer childKey, questitem whichQuestitem returns boolean

native  SaveDefeatConditionHandle		takes hashtable table, integer parentKey, integer childKey, defeatcondition whichDefeatcondition returns boolean

native  SaveTimerDialogHandle			takes hashtable table, integer parentKey, integer childKey, timerdialog whichTimerdialog returns boolean

native  SaveLeaderboardHandle			takes hashtable table, integer parentKey, integer childKey, leaderboard whichLeaderboard returns boolean

native  SaveMultiboardHandle			takes hashtable table, integer parentKey, integer childKey, multiboard whichMultiboard returns boolean

native  SaveMultiboardItemHandle		takes hashtable table, integer parentKey, integer childKey, multiboarditem whichMultiboarditem returns boolean

native  SaveTrackableHandle				takes hashtable table, integer parentKey, integer childKey, trackable whichTrackable returns boolean

native  SaveDialogHandle				takes hashtable table, integer parentKey, integer childKey, dialog whichDialog returns boolean

native  SaveButtonHandle				takes hashtable table, integer parentKey, integer childKey, button whichButton returns boolean

native  SaveTextTagHandle				takes hashtable table, integer parentKey, integer childKey, texttag whichTexttag returns boolean

native  SaveLightningHandle				takes hashtable table, integer parentKey, integer childKey, lightning whichLightning returns boolean

native  SaveImageHandle					takes hashtable table, integer parentKey, integer childKey, image whichImage returns boolean

native  SaveUbersplatHandle				takes hashtable table, integer parentKey, integer childKey, ubersplat whichUbersplat returns boolean

native  SaveRegionHandle				takes hashtable table, integer parentKey, integer childKey, region whichRegion returns boolean

native  SaveFogStateHandle				takes hashtable table, integer parentKey, integer childKey, fogstate whichFogState returns boolean

native  SaveFogModifierHandle			takes hashtable table, integer parentKey, integer childKey, fogmodifier whichFogModifier returns boolean

native  SaveAgentHandle					takes hashtable table, integer parentKey, integer childKey, agent whichAgent returns boolean

native  SaveHashtableHandle				takes hashtable table, integer parentKey, integer childKey, hashtable whichHashtable returns boolean





native  LoadInteger					takes hashtable table, integer parentKey, integer childKey returns integer

native  LoadReal					takes hashtable table, integer parentKey, integer childKey returns real

native  LoadBoolean				    takes hashtable table, integer parentKey, integer childKey returns boolean

native  LoadStr 					takes hashtable table, integer parentKey, integer childKey returns string

native  LoadPlayerHandle			takes hashtable table, integer parentKey, integer childKey returns player

native  LoadWidgetHandle			takes hashtable table, integer parentKey, integer childKey returns widget

native  LoadDestructableHandle		takes hashtable table, integer parentKey, integer childKey returns destructable

native  LoadItemHandle				takes hashtable table, integer parentKey, integer childKey returns item

native  LoadUnitHandle				takes hashtable table, integer parentKey, integer childKey returns unit

native  LoadAbilityHandle			takes hashtable table, integer parentKey, integer childKey returns ability

native  LoadTimerHandle				takes hashtable table, integer parentKey, integer childKey returns timer

native  LoadTriggerHandle			takes hashtable table, integer parentKey, integer childKey returns trigger

native  LoadTriggerConditionHandle	takes hashtable table, integer parentKey, integer childKey returns triggercondition

native  LoadTriggerActionHandle		takes hashtable table, integer parentKey, integer childKey returns triggeraction

native  LoadTriggerEventHandle		takes hashtable table, integer parentKey, integer childKey returns event

native  LoadForceHandle				takes hashtable table, integer parentKey, integer childKey returns force

native  LoadGroupHandle				takes hashtable table, integer parentKey, integer childKey returns group

native  LoadLocationHandle			takes hashtable table, integer parentKey, integer childKey returns location

native  LoadRectHandle				takes hashtable table, integer parentKey, integer childKey returns rect

native  LoadBooleanExprHandle		takes hashtable table, integer parentKey, integer childKey returns boolexpr

native  LoadSoundHandle				takes hashtable table, integer parentKey, integer childKey returns sound

native  LoadEffectHandle			takes hashtable table, integer parentKey, integer childKey returns effect

native  LoadUnitPoolHandle			takes hashtable table, integer parentKey, integer childKey returns unitpool

native  LoadItemPoolHandle			takes hashtable table, integer parentKey, integer childKey returns itempool

native  LoadQuestHandle				takes hashtable table, integer parentKey, integer childKey returns quest

native  LoadQuestItemHandle			takes hashtable table, integer parentKey, integer childKey returns questitem

native  LoadDefeatConditionHandle	takes hashtable table, integer parentKey, integer childKey returns defeatcondition

native  LoadTimerDialogHandle		takes hashtable table, integer parentKey, integer childKey returns timerdialog

native  LoadLeaderboardHandle		takes hashtable table, integer parentKey, integer childKey returns leaderboard

native  LoadMultiboardHandle		takes hashtable table, integer parentKey, integer childKey returns multiboard

native  LoadMultiboardItemHandle	takes hashtable table, integer parentKey, integer childKey returns multiboarditem

native  LoadTrackableHandle			takes hashtable table, integer parentKey, integer childKey returns trackable

native  LoadDialogHandle			takes hashtable table, integer parentKey, integer childKey returns dialog

native  LoadButtonHandle			takes hashtable table, integer parentKey, integer childKey returns button

native  LoadTextTagHandle			takes hashtable table, integer parentKey, integer childKey returns texttag

native  LoadLightningHandle			takes hashtable table, integer parentKey, integer childKey returns lightning

native  LoadImageHandle				takes hashtable table, integer parentKey, integer childKey returns image

native  LoadUbersplatHandle			takes hashtable table, integer parentKey, integer childKey returns ubersplat

native  LoadRegionHandle			takes hashtable table, integer parentKey, integer childKey returns region

native  LoadFogStateHandle			takes hashtable table, integer parentKey, integer childKey returns fogstate

native  LoadFogModifierHandle		takes hashtable table, integer parentKey, integer childKey returns fogmodifier

native  LoadHashtableHandle			takes hashtable table, integer parentKey, integer childKey returns hashtable



native  HaveSavedInteger					takes hashtable table, integer parentKey, integer childKey returns boolean

native  HaveSavedReal						takes hashtable table, integer parentKey, integer childKey returns boolean

native  HaveSavedBoolean					takes hashtable table, integer parentKey, integer childKey returns boolean

native  HaveSavedString					    takes hashtable table, integer parentKey, integer childKey returns boolean

native  HaveSavedHandle     				takes hashtable table, integer parentKey, integer childKey returns boolean



native  RemoveSavedInteger					takes hashtable table, integer parentKey, integer childKey returns nothing

native  RemoveSavedReal						takes hashtable table, integer parentKey, integer childKey returns nothing

native  RemoveSavedBoolean					takes hashtable table, integer parentKey, integer childKey returns nothing

native  RemoveSavedString					takes hashtable table, integer parentKey, integer childKey returns nothing

native  RemoveSavedHandle					takes hashtable table, integer parentKey, integer childKey returns nothing



native  FlushParentHashtable						takes hashtable table returns nothing

native  FlushChildHashtable					takes hashtable table, integer parentKey returns nothing
