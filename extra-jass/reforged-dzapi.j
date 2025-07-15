// These are "DzAPI" natives are an extension that only exist on the Chinese NetEase Warcraft 3 platform.
// Reforged exports these functions (visible in Lua), but they are probably dysfunctional outside of NetEase.
// If the NetEase client has additional natives, these don't belong here.
// Only what Blizzard's worldwide Reforged build has is listed below.
// DzAPI is particularly of interest, if you want to translate and port Chinese maps to other platforms.


/**

*/
native DzAPI_Map_ChangeStoreItemCoolDown takes nothing returns nothing // placeholder. function signature unknown!


/**

*/
native DzAPI_Map_ChangeStoreItemCount    takes nothing returns nothing // placeholder. function signature unknown!


/**

*/
native DzAPI_Map_GetActivityData         takes nothing returns string


/**

*/
native DzAPI_Map_GetGameStartTime        takes nothing returns integer


/**

*/
native DzAPI_Map_GetGuildName            takes player whichPlayer returns string


/**

*/
native DzAPI_Map_GetGuildRole            takes player whichPlayer returns integer


/**

*/
native DzAPI_Map_GetLadderLevel          takes player whichPlayer returns integer


/**

*/
native DzAPI_Map_GetLadderRank           takes player whichPlayer returns integer


/**

*/
native DzAPI_Map_GetMapConfig            takes string key returns string


/**

*/
native DzAPI_Map_GetMapLevel             takes player whichPlayer returns integer // can be substituted with return 99


/**

*/
native DzAPI_Map_GetMapLevelRank         takes player whichPlayer returns integer


/**

*/
native DzAPI_Map_GetMatchType      		 takes nothing returns integer


/**

*/
native DzAPI_Map_GetPlatformVIP          takes player whichPlayer returns integer // can be substituted with return 100


/**

*/
native DzAPI_Map_GetPublicArchive        takes player whichPlayer, string key returns string


/**

*/
native DzAPI_Map_GetServerArchiveDrop    takes player whichPlayer, string key returns string


/**

*/
native DzAPI_Map_GetServerArchiveEquip   takes player whichPlayer, string key returns integer


/**

*/
native DzAPI_Map_GetServerValue          takes player whichPlayer, string key returns string


/**

*/
native DzAPI_Map_GetServerValueErrorCode takes player whichPlayer returns integer


/**

*/
native DzAPI_Map_GetUserID               takes nothing returns nothing // placeholder. function signature unknown!


/**

*/
native DzAPI_Map_HasMallItem             takes player whichPlayer, string key returns boolean // can be substituted with return true


/**

*/
native DzAPI_Map_IsBlueVIP               takes player whichPlayer returns boolean


/**

*/
native DzAPI_Map_IsRPGLadder             takes nothing returns boolean


/**

*/
native DzAPI_Map_IsRPGLobby              takes nothing returns boolean


/**

*/
native DzAPI_Map_IsRedVIP                takes player whichPlayer returns boolean


/**

*/
native DzAPI_Map_Ladder_SetPlayerStat    takes player whichPlayer, string key, string value returns nothing


/**

*/
native DzAPI_Map_Ladder_SetStat          takes player whichPlayer, string key, string value returns nothing


/**

*/
native DzAPI_Map_MissionComplete         takes player whichPlayer, string key, string value returns nothing


/**

*/
native DzAPI_Map_OrpgTrigger             takes player whichPlayer, string key returns nothing


/**

*/
native DzAPI_Map_SavePublicArchive       takes player whichPlayer, string key, string value returns boolean


/**

*/
native DzAPI_Map_SaveServerValue         takes player whichPlayer, string key, string value returns boolean


/**

*/
native DzAPI_Map_Stat_SetStat            takes player whichPlayer, string key, string value returns nothing


/**
Used for Platform statistics:
"Generally used to count the number of times certain events are triggered in the game, which can be viewed in author's profile."

@param eventKey "embedding key". Guessed type as string, please confirm.
@param subkey leave blank. Reserved for future use.
@param times guessed type as integer, please confirm.
@return guessed type as integer, please confirm.
*/
native DzAPI_Map_Statistics              takes player whichPlayer, string eventKey, string subKey, integer times returns integer


/**

*/
native DzAPI_Map_ToggleStore             takes nothing returns nothing // placeholder. function signature unknown!


/**

*/
native DzAPI_Map_UpdatePlayerHero        takes nothing returns nothing // placeholder. function signature unknown!


/**

*/
native DzAPI_Map_UseConsumablesItem      takes player whichPlayer, string key returns nothing

