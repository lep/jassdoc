// Campaign API

native  SetTutorialCleared      takes boolean cleared returns nothing

native  SetMissionAvailable     takes integer campaignNumber, integer missionNumber, boolean available returns nothing

native  SetCampaignAvailable    takes integer campaignNumber, boolean available  returns nothing

native  SetOpCinematicAvailable takes integer campaignNumber, boolean available  returns nothing

native  SetEdCinematicAvailable takes integer campaignNumber, boolean available  returns nothing

native  GetDefaultDifficulty    takes nothing returns gamedifficulty

native  SetDefaultDifficulty    takes gamedifficulty g returns nothing

native  SetCustomCampaignButtonVisible  takes integer whichButton, boolean visible returns nothing

native  GetCustomCampaignButtonVisible  takes integer whichButton returns boolean

native  DoNotSaveReplay         takes nothing returns nothing