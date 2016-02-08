// Game API

native VersionGet takes nothing returns version

native VersionCompatible takes version whichVersion returns boolean

native VersionSupported takes version whichVersion returns boolean



native EndGame takes boolean doScoreScreen returns nothing



// Async only!

native          ChangeLevel         takes string newLevel, boolean doScoreScreen returns nothing

native          RestartGame         takes boolean doScoreScreen returns nothing

native          ReloadGame          takes nothing returns nothing

/**
@note Deprecated. Use SetCampaignMenuRaceEx instead.
*/
native          SetCampaignMenuRace takes race r returns nothing

native          SetCampaignMenuRaceEx takes integer campaignIndex returns nothing

native          ForceCampaignSelectScreen takes nothing returns nothing



native          LoadGame            takes string saveFileName, boolean doScoreScreen returns nothing

native          SaveGame            takes string saveFileName returns nothing

native          RenameSaveDirectory takes string sourceDirName, string destDirName returns boolean

native          RemoveSaveDirectory takes string sourceDirName returns boolean

native          CopySaveGame        takes string sourceSaveName, string destSaveName returns boolean

native          SaveGameExists      takes string saveName returns boolean

native          SyncSelections      takes nothing returns nothing

native          SetFloatGameState   takes fgamestate whichFloatGameState, real value returns nothing

constant native GetFloatGameState   takes fgamestate whichFloatGameState returns real

native          SetIntegerGameState takes igamestate whichIntegerGameState, integer value returns nothing

constant native GetIntegerGameState takes igamestate whichIntegerGameState returns integer