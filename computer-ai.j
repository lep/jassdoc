// Computer AI interface

native StartMeleeAI         takes player num, string script                 returns nothing

native StartCampaignAI      takes player num, string script                 returns nothing

native CommandAI            takes player num, integer command, integer data returns nothing

native PauseCompAI          takes player p,   boolean pause                 returns nothing

native GetAIDifficulty      takes player num                                returns aidifficulty



native RemoveGuardPosition  takes unit hUnit                                returns nothing

native RecycleGuardPosition takes unit hUnit                                returns nothing

native RemoveAllGuardPositions takes player num                             returns nothing
