
/**
@note You cannot create more than 255 hashtables
@patch 1.24
*/
native  InitHashtable    takes nothing returns hashtable



/**
@patch 1.24
*/
native  SaveInteger						takes hashtable table, integer parentKey, integer childKey, integer value returns nothing

/**
@patch 1.24
*/
native  SaveReal						takes hashtable table, integer parentKey, integer childKey, real value returns nothing

/**
@patch 1.24
*/
native  SaveBoolean						takes hashtable table, integer parentKey, integer childKey, boolean value returns nothing

/**
@patch 1.24
*/
native  SaveStr							takes hashtable table, integer parentKey, integer childKey, string value returns boolean

/**
@patch 1.24
*/
native  SavePlayerHandle				takes hashtable table, integer parentKey, integer childKey, player whichPlayer returns boolean

/**
@patch 1.24
*/
native  SaveWidgetHandle				takes hashtable table, integer parentKey, integer childKey, widget whichWidget returns boolean

/**
@patch 1.24
*/
native  SaveDestructableHandle			takes hashtable table, integer parentKey, integer childKey, destructable whichDestructable returns boolean

/**
@patch 1.24
*/
native  SaveItemHandle					takes hashtable table, integer parentKey, integer childKey, item whichItem returns boolean

/**
@patch 1.24
*/
native  SaveUnitHandle					takes hashtable table, integer parentKey, integer childKey, unit whichUnit returns boolean

/**
@patch 1.24
*/
native  SaveAbilityHandle				takes hashtable table, integer parentKey, integer childKey, ability whichAbility returns boolean

/**
@patch 1.24
*/
native  SaveTimerHandle					takes hashtable table, integer parentKey, integer childKey, timer whichTimer returns boolean

/**
@patch 1.24
*/
native  SaveTriggerHandle				takes hashtable table, integer parentKey, integer childKey, trigger whichTrigger returns boolean

/**
@patch 1.24
*/
native  SaveTriggerConditionHandle		takes hashtable table, integer parentKey, integer childKey, triggercondition whichTriggercondition returns boolean

/**
@patch 1.24
*/
native  SaveTriggerActionHandle			takes hashtable table, integer parentKey, integer childKey, triggeraction whichTriggeraction returns boolean

/**
@patch 1.24
*/
native  SaveTriggerEventHandle			takes hashtable table, integer parentKey, integer childKey, event whichEvent returns boolean

/**
@patch 1.24
*/
native  SaveForceHandle					takes hashtable table, integer parentKey, integer childKey, force whichForce returns boolean

/**
@patch 1.24
*/
native  SaveGroupHandle					takes hashtable table, integer parentKey, integer childKey, group whichGroup returns boolean

/**
@patch 1.24
*/
native  SaveLocationHandle				takes hashtable table, integer parentKey, integer childKey, location whichLocation returns boolean

/**
@patch 1.24
*/
native  SaveRectHandle					takes hashtable table, integer parentKey, integer childKey, rect whichRect returns boolean

/**
@patch 1.24
*/
native  SaveBooleanExprHandle			takes hashtable table, integer parentKey, integer childKey, boolexpr whichBoolexpr returns boolean

/**
@patch 1.24
*/
native  SaveSoundHandle					takes hashtable table, integer parentKey, integer childKey, sound whichSound returns boolean

/**
@patch 1.24
*/
native  SaveEffectHandle				takes hashtable table, integer parentKey, integer childKey, effect whichEffect returns boolean

/**
@patch 1.24
*/
native  SaveUnitPoolHandle				takes hashtable table, integer parentKey, integer childKey, unitpool whichUnitpool returns boolean

/**
@patch 1.24
*/
native  SaveItemPoolHandle				takes hashtable table, integer parentKey, integer childKey, itempool whichItempool returns boolean

/**
@patch 1.24
*/
native  SaveQuestHandle					takes hashtable table, integer parentKey, integer childKey, quest whichQuest returns boolean

/**
@patch 1.24
*/
native  SaveQuestItemHandle				takes hashtable table, integer parentKey, integer childKey, questitem whichQuestitem returns boolean

/**
@patch 1.24
*/
native  SaveDefeatConditionHandle		takes hashtable table, integer parentKey, integer childKey, defeatcondition whichDefeatcondition returns boolean

/**
@patch 1.24
*/
native  SaveTimerDialogHandle			takes hashtable table, integer parentKey, integer childKey, timerdialog whichTimerdialog returns boolean

/**
@patch 1.24
*/
native  SaveLeaderboardHandle			takes hashtable table, integer parentKey, integer childKey, leaderboard whichLeaderboard returns boolean

/**
@patch 1.24
*/
native  SaveMultiboardHandle			takes hashtable table, integer parentKey, integer childKey, multiboard whichMultiboard returns boolean

/**
@patch 1.24
*/
native  SaveMultiboardItemHandle		takes hashtable table, integer parentKey, integer childKey, multiboarditem whichMultiboarditem returns boolean

/**
@patch 1.24
*/
native  SaveTrackableHandle				takes hashtable table, integer parentKey, integer childKey, trackable whichTrackable returns boolean

/**
@patch 1.24
*/
native  SaveDialogHandle				takes hashtable table, integer parentKey, integer childKey, dialog whichDialog returns boolean

/**
@patch 1.24
*/
native  SaveButtonHandle				takes hashtable table, integer parentKey, integer childKey, button whichButton returns boolean

/**
@patch 1.24
*/
native  SaveTextTagHandle				takes hashtable table, integer parentKey, integer childKey, texttag whichTexttag returns boolean

/**
@patch 1.24
*/
native  SaveLightningHandle				takes hashtable table, integer parentKey, integer childKey, lightning whichLightning returns boolean

/**
@patch 1.24
*/
native  SaveImageHandle					takes hashtable table, integer parentKey, integer childKey, image whichImage returns boolean

/**
@patch 1.24
*/
native  SaveUbersplatHandle				takes hashtable table, integer parentKey, integer childKey, ubersplat whichUbersplat returns boolean

/**
@patch 1.24
*/
native  SaveRegionHandle				takes hashtable table, integer parentKey, integer childKey, region whichRegion returns boolean

/**
@patch 1.24
*/
native  SaveFogStateHandle				takes hashtable table, integer parentKey, integer childKey, fogstate whichFogState returns boolean

/**
@patch 1.24
*/
native  SaveFogModifierHandle			takes hashtable table, integer parentKey, integer childKey, fogmodifier whichFogModifier returns boolean

/**
@patch 1.24b
*/
native  SaveAgentHandle					takes hashtable table, integer parentKey, integer childKey, agent whichAgent returns boolean

/**
@patch 1.24b
*/
native  SaveHashtableHandle				takes hashtable table, integer parentKey, integer childKey, hashtable whichHashtable returns boolean

/**
@patch 1.31
*/
native  SaveFrameHandle					takes hashtable table, integer parentKey, integer childKey, framehandle whichFrameHandle returns boolean





/**
@patch 1.24
*/
native  LoadInteger					takes hashtable table, integer parentKey, integer childKey returns integer

/**
@patch 1.24
*/
native  LoadReal					takes hashtable table, integer parentKey, integer childKey returns real

/**
@patch 1.24
*/
native  LoadBoolean				    takes hashtable table, integer parentKey, integer childKey returns boolean

/**
@patch 1.24
*/
native  LoadStr 					takes hashtable table, integer parentKey, integer childKey returns string

/**
@patch 1.24
*/
native  LoadPlayerHandle			takes hashtable table, integer parentKey, integer childKey returns player

/**
@patch 1.24
*/
native  LoadWidgetHandle			takes hashtable table, integer parentKey, integer childKey returns widget

/**
@patch 1.24
*/
native  LoadDestructableHandle		takes hashtable table, integer parentKey, integer childKey returns destructable

/**
@patch 1.24
*/
native  LoadItemHandle				takes hashtable table, integer parentKey, integer childKey returns item

/**
@patch 1.24
*/
native  LoadUnitHandle				takes hashtable table, integer parentKey, integer childKey returns unit

/**
@patch 1.24
*/
native  LoadAbilityHandle			takes hashtable table, integer parentKey, integer childKey returns ability

/**
@patch 1.24
*/
native  LoadTimerHandle				takes hashtable table, integer parentKey, integer childKey returns timer

/**
@patch 1.24
*/
native  LoadTriggerHandle			takes hashtable table, integer parentKey, integer childKey returns trigger

/**
@patch 1.24
*/
native  LoadTriggerConditionHandle	takes hashtable table, integer parentKey, integer childKey returns triggercondition

/**
@patch 1.24
*/
native  LoadTriggerActionHandle		takes hashtable table, integer parentKey, integer childKey returns triggeraction

/**
@patch 1.24
*/
native  LoadTriggerEventHandle		takes hashtable table, integer parentKey, integer childKey returns event

/**
@patch 1.24
*/
native  LoadForceHandle				takes hashtable table, integer parentKey, integer childKey returns force

/**
@patch 1.24
*/
native  LoadGroupHandle				takes hashtable table, integer parentKey, integer childKey returns group

/**
@patch 1.24
*/
native  LoadLocationHandle			takes hashtable table, integer parentKey, integer childKey returns location

/**
@patch 1.24
*/
native  LoadRectHandle				takes hashtable table, integer parentKey, integer childKey returns rect

/**
@patch 1.24
*/
native  LoadBooleanExprHandle		takes hashtable table, integer parentKey, integer childKey returns boolexpr

/**
@patch 1.24
*/
native  LoadSoundHandle				takes hashtable table, integer parentKey, integer childKey returns sound

/**
@patch 1.24
*/
native  LoadEffectHandle			takes hashtable table, integer parentKey, integer childKey returns effect

/**
@patch 1.24
*/
native  LoadUnitPoolHandle			takes hashtable table, integer parentKey, integer childKey returns unitpool

/**
@patch 1.24
*/
native  LoadItemPoolHandle			takes hashtable table, integer parentKey, integer childKey returns itempool

/**
@patch 1.24
*/
native  LoadQuestHandle				takes hashtable table, integer parentKey, integer childKey returns quest

/**
@patch 1.24
*/
native  LoadQuestItemHandle			takes hashtable table, integer parentKey, integer childKey returns questitem

/**
@patch 1.24
*/
native  LoadDefeatConditionHandle	takes hashtable table, integer parentKey, integer childKey returns defeatcondition

/**
@patch 1.24
*/
native  LoadTimerDialogHandle		takes hashtable table, integer parentKey, integer childKey returns timerdialog

/**
@patch 1.24
*/
native  LoadLeaderboardHandle		takes hashtable table, integer parentKey, integer childKey returns leaderboard

/**
@patch 1.24
*/
native  LoadMultiboardHandle		takes hashtable table, integer parentKey, integer childKey returns multiboard

/**
@patch 1.24
*/
native  LoadMultiboardItemHandle	takes hashtable table, integer parentKey, integer childKey returns multiboarditem

/**
@patch 1.24
*/
native  LoadTrackableHandle			takes hashtable table, integer parentKey, integer childKey returns trackable

/**
@patch 1.24
*/
native  LoadDialogHandle			takes hashtable table, integer parentKey, integer childKey returns dialog

/**
@patch 1.24
*/
native  LoadButtonHandle			takes hashtable table, integer parentKey, integer childKey returns button

/**
@patch 1.24
*/
native  LoadTextTagHandle			takes hashtable table, integer parentKey, integer childKey returns texttag

/**
@patch 1.24
*/
native  LoadLightningHandle			takes hashtable table, integer parentKey, integer childKey returns lightning

/**
@patch 1.24
*/
native  LoadImageHandle				takes hashtable table, integer parentKey, integer childKey returns image

/**
@patch 1.24
*/
native  LoadUbersplatHandle			takes hashtable table, integer parentKey, integer childKey returns ubersplat

/**
@patch 1.24
*/
native  LoadRegionHandle			takes hashtable table, integer parentKey, integer childKey returns region

/**
@patch 1.24
*/
native  LoadFogStateHandle			takes hashtable table, integer parentKey, integer childKey returns fogstate

/**
@patch 1.24
*/
native  LoadFogModifierHandle		takes hashtable table, integer parentKey, integer childKey returns fogmodifier

/**
@patch 1.24
*/
native  LoadHashtableHandle			takes hashtable table, integer parentKey, integer childKey returns hashtable

/**
@patch 1.31
*/
native  LoadFrameHandle				takes hashtable table, integer parentKey, integer childKey returns framehandle


/**
@patch 1.24
*/
native  HaveSavedInteger					takes hashtable table, integer parentKey, integer childKey returns boolean

/**
@patch 1.24
*/
native  HaveSavedReal						takes hashtable table, integer parentKey, integer childKey returns boolean

/**
@patch 1.24
*/
native  HaveSavedBoolean					takes hashtable table, integer parentKey, integer childKey returns boolean

/**
@patch 1.24
*/
native  HaveSavedString					    takes hashtable table, integer parentKey, integer childKey returns boolean

/**
@patch 1.24
*/
native  HaveSavedHandle     				takes hashtable table, integer parentKey, integer childKey returns boolean



/**
@patch 1.24
*/
native  RemoveSavedInteger					takes hashtable table, integer parentKey, integer childKey returns nothing

/**
@patch 1.24
*/
native  RemoveSavedReal						takes hashtable table, integer parentKey, integer childKey returns nothing

/**
@patch 1.24
*/
native  RemoveSavedBoolean					takes hashtable table, integer parentKey, integer childKey returns nothing

/**
@patch 1.24
*/
native  RemoveSavedString					takes hashtable table, integer parentKey, integer childKey returns nothing

/**
@patch 1.24
*/
native  RemoveSavedHandle					takes hashtable table, integer parentKey, integer childKey returns nothing



/**
@patch 1.24
*/
native  FlushParentHashtable						takes hashtable table returns nothing

/**
@patch 1.24
*/
native  FlushChildHashtable					takes hashtable table, integer parentKey returns nothing
