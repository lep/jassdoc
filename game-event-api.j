// Trigger Game Event API

native TriggerRegisterVariableEvent takes trigger whichTrigger, string varName, limitop opcode, real limitval returns event



    // EVENT_GAME_VARIABLE_LIMIT

    //constant native string GetTriggeringVariableName takes nothing returns string


/**
Creates it's own timer and triggers when it expires
*/
native TriggerRegisterTimerEvent takes trigger whichTrigger, real timeout, boolean periodic returns event


/**
Triggers when the timer you tell it about expires
*/
native TriggerRegisterTimerExpireEvent takes trigger whichTrigger, timer t returns event



native TriggerRegisterGameStateEvent takes trigger whichTrigger, gamestate whichState, limitop opcode, real limitval returns event


// http://www.wc3c.net/showthread.php?p=1122625
// https://github.com/nvs/gem/commit/4f40b178327699ce7052f8db786f57028317d208
// cannot reproduce in single player?
native TriggerRegisterDialogEvent       takes trigger whichTrigger, dialog whichDialog returns event

native TriggerRegisterDialogButtonEvent takes trigger whichTrigger, button whichButton returns event



/**
@event EVENT_GAME_STATE_LIMIT
*/
constant native GetEventGameState takes nothing returns gamestate



native TriggerRegisterGameEvent takes trigger whichTrigger, gameevent whichGameEvent returns event

  

/**
@event EVENT_GAME_VICTORY
*/
constant native GetWinningPlayer takes nothing returns player





native TriggerRegisterEnterRegion takes trigger whichTrigger, region whichRegion, boolexpr filter returns event



/**
@event EVENT_GAME_ENTER_REGION
*/
constant native GetTriggeringRegion takes nothing returns region

/**
@event EVENT_GAME_ENTER_REGION
*/
constant native GetEnteringUnit takes nothing returns unit







native TriggerRegisterLeaveRegion takes trigger whichTrigger, region whichRegion, boolexpr filter returns event

/**
@event EVENT_GAME_LEAVE_REGION
*/
constant native GetLeavingUnit takes nothing returns unit



native TriggerRegisterTrackableHitEvent takes trigger whichTrigger, trackable t returns event

native TriggerRegisterTrackableTrackEvent takes trigger whichTrigger, trackable t returns event


/**
@event EVENT_GAME_TRACKABLE_HIT
@event EVENT_GAME_TRACKABLE_TRACK
*/
constant native GetTriggeringTrackable takes nothing returns trackable



/**
@event EVENT_DIALOG_BUTTON_CLICK
*/
constant native GetClickedButton takes nothing returns button

/**
@event EVENT_DIALOG_BUTTON_CLICK
*/
constant native GetClickedDialog    takes nothing returns dialog


/**
@event EVENT_GAME_TOURNAMENT_FINISH_SOON
*/
constant native GetTournamentFinishSoonTimeRemaining takes nothing returns real

/**
@event EVENT_GAME_TOURNAMENT_FINISH_SOON
*/
constant native GetTournamentFinishNowRule takes nothing returns integer

/**
@event EVENT_GAME_TOURNAMENT_FINISH_SOON
*/
constant native GetTournamentFinishNowPlayer takes nothing returns player

/**
@event EVENT_GAME_TOURNAMENT_FINISH_SOON
*/
constant native GetTournamentScore takes player whichPlayer returns integer


/**
@event EVENT_GAME_SAVE
*/
constant native GetSaveBasicFilename takes nothing returns string

