// Game API

native VersionGet takes nothing returns version

native VersionCompatible takes version whichVersion returns boolean

native VersionSupported takes version whichVersion returns boolean



native EndGame takes boolean doScoreScreen returns nothing



// Async only!

/**
Loads the next level for all players. Note that this function is asynchronous,
so each player will be sent to their own map. If the boolean is set to true,
the score screen will appear before the user progresses to the next level.

@param newLevel
The path of the next level. The path is relative to the Warcraft III folder.

@param doScoreScreen
If set to true, the score screen will appear before the user progresses to the next level.
*/
native          ChangeLevel         takes string newLevel, boolean doScoreScreen returns nothing

native          RestartGame         takes boolean doScoreScreen returns nothing

native          ReloadGame          takes nothing returns nothing

/**
@note Deprecated. Use SetCampaignMenuRaceEx instead.
*/
native          SetCampaignMenuRace takes race r returns nothing

native          SetCampaignMenuRaceEx takes integer campaignIndex returns nothing

native          ForceCampaignSelectScreen takes nothing returns nothing


/**
@bug The filename seems to have some limitations:

- No underscores in campaign names.
- Shorter file names for savegames.
- Probably no dots in savegames or campaign names.

For more info see <http://www.hiveworkshop.com/threads/map-transition-does-not-work-when-loading-a-custom-savegame.286927/>.
*/
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


/**
@patch 1.32
*/
native          SetMaxCheckpointSaves  	takes integer maxCheckpointSaves returns nothing

/**
@patch 1.32
*/
native          SaveGameCheckpoint  	takes string saveFileName, boolean showWindow returns nothing
