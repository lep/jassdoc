// These are "DzAPI" natives are an extension that only exist on the Chinese NetEase Warcraft 3 platform.
// Reforged exports these functions (visible in Lua), but they are probably dysfunctional outside of NetEase.
// If the NetEase client has additional natives, these don't belong here.
// Only what Blizzard's worldwide Reforged build has is listed below.
// DzAPI is particularly of interest, if you want to translate and port Chinese maps to other platforms.


/**
Alias of `RequestExtraXXXXXData(24, null, itemID, null, false, team, seconds, 0)`
*/
native DzAPI_Map_ChangeStoreItemCoolDown takes integer team, string itemID, integer seconds returns nothing


/**
Alias of `RequestExtraXXXXXData(23, null, itemID, null, false, team, count, 0)`
*/
native DzAPI_Map_ChangeStoreItemCount    takes integer team, string itemID, integer count returns nothing


/**
Alias of `RequestExtraStringData(2, null, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetActivityData         takes nothing returns string


/**
Alias of `RequestExtraIntegerData(11, null, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetGameStartTime        takes nothing returns integer


/**
Alias of `RequestExtraStringData(19, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetGuildName            takes player whichPlayer returns string


/**
Alias of `RequestExtraIntegerData(20, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetGuildRole            takes player whichPlayer returns integer


/**
Alias of `RequestExtraIntegerData(14, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetLadderLevel          takes player whichPlayer returns integer


/**
Alias of `RequestExtraIntegerData(17, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetLadderRank           takes player whichPlayer returns integer


/**
Alias of `RequestExtraStringData(21, null, key, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetMapConfig            takes string key returns string


/**
Alias of `RequestExtraIntegerData(3, whichPlayer, null, null, false, 0, 0, 0)`

Can be substituted with `return 99`.
*/
native DzAPI_Map_GetMapLevel             takes player whichPlayer returns integer


/**
Alias of `RequestExtraIntegerData(18, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetMapLevelRank         takes player whichPlayer returns integer


/**
Alias of `RequestExtraIntegerData(13, null, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetMatchType      		 takes nothing returns integer


/**
Alias of `RequestExtraIntegerData(30, whichPlayer, null, null, false, 0, 0, 0)`

Can be substituted with `return 100`.
*/
native DzAPI_Map_GetPlatformVIP          takes player whichPlayer returns integer


/**
Alias of `RequestExtraStringData(32, whichPlayer, key, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetPublicArchive        takes player whichPlayer, string key returns string


/**
Alias of `RequestExtraStringData(27, whichPlayer, key, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetServerArchiveDrop    takes player whichPlayer, string key returns string


/**
Alias of `RequestExtraIntegerData(26, whichPlayer, key, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetServerArchiveEquip   takes player whichPlayer, string key returns integer


/**
Alias of `RequestExtraStringData(5, whichPlayer, key, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetServerValue          takes player whichPlayer, string key returns string


/**
Alias of `RequestExtraIntegerData(6, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetServerValueErrorCode takes player whichPlayer returns integer


/**
Alias of `RequestExtraIntegerData(29, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_GetUserID               takes player whichPlayer returns integer


/**
Alias of `RequestExtraIntegerData(22, whichPlayer, key, null, false, 0, 0, 0) != 0`

Can be substituted with `return true`.
*/
native DzAPI_Map_HasMallItem             takes player whichPlayer, string key returns boolean


/**
Alias of `RequestExtraBooleanData(16, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_IsBlueVIP               takes player whichPlayer returns boolean


/**
Alias of `RequestExtraBooleanData(12, null, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_IsRPGLadder             takes nothing returns boolean


/**
Alias of `RequestExtraBooleanData(10, null, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_IsRPGLobby              takes nothing returns boolean


/**
Alias of `RequestExtraBooleanData(15, whichPlayer, null, null, false, 0, 0, 0)`
*/
native DzAPI_Map_IsRedVIP                takes player whichPlayer returns boolean


/**
Alias of `RequestExtraXXXXXData(9, whichPlayer, key, value, false, 0, 0, 0)`
*/
native DzAPI_Map_Ladder_SetPlayerStat    takes player whichPlayer, string key, string value returns nothing


/**
Alias of `RequestExtraXXXXXData(8, whichPlayer, key, value, false, 0, 0, 0)`
*/
native DzAPI_Map_Ladder_SetStat          takes player whichPlayer, string key, string value returns nothing


/**
Alias of `RequestExtraXXXXXData(1, whichPlayer, key, value, false, 0, 0, 0)`
*/
native DzAPI_Map_MissionComplete         takes player whichPlayer, string key, string value returns nothing


/**
Alias of `RequestExtraXXXXXData(28, whichPlayer, key, null, false, 0, 0, 0)`
*/
native DzAPI_Map_OrpgTrigger             takes player whichPlayer, string key returns nothing


/**
Alias of `RequestExtraBooleanData(31, whichPlayer, key, value, false, 0, 0, 0)`
*/
native DzAPI_Map_SavePublicArchive       takes player whichPlayer, string key, string value returns boolean


/**
Alias of `RequestExtraBooleanData(4, whichPlayer, key, value, false, 0, 0, 0)`
*/
native DzAPI_Map_SaveServerValue         takes player whichPlayer, string key, string value returns boolean


/**
Alias of `RequestExtraXXXXXData(7, whichPlayer, key, value, false, 0, 0, 0)`
*/
native DzAPI_Map_Stat_SetStat            takes player whichPlayer, string key, string value returns nothing


/**
Used for Platform statistics:
"Generally used to count the number of times certain events are triggered in the game, which can be viewed in author's profile."

Alias of `RequestExtraXXXXXData(34, whichPlayer, category, label, false, 0, 0, 0)`

@param category "embedding key". Guessed type as string, please confirm.
@param label leave blank. Reserved for future use.
*/
native DzAPI_Map_Statistics              takes player whichPlayer, string category, string label returns nothing


/**
Alias of `RequestExtraXXXXXData(25, whichPlayer, null, null, show, 0, 0, 0)`
*/
native DzAPI_Map_ToggleStore             takes player whichPlayer, boolean show returns nothing


/**
Does nothing at all.
*/
native DzAPI_Map_UpdatePlayerHero        takes player whichPlayer, unit whichUnit returns nothing


/**
Alias of `call RequestExtraXXXXXData(33, whichPlayer, key, null, false, 0, 0, 0)`
*/
native DzAPI_Map_UseConsumablesItem      takes player whichPlayer, string key returns nothing

