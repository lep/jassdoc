// Trigger Game Event API

native TriggerRegisterVariableEvent takes trigger whichTrigger, string varName, limitop opcode, real limitval returns event



    // EVENT_GAME_VARIABLE_LIMIT

    //constant native string GetTriggeringVariableName takes nothing returns string


/**
Creates its own timer and triggers when it expires.

@note The table below shows how often `TriggerRegisterTimerEvent` aka 
`TriggerRegisterTimerEventPeriodic` is executed per second with different
timeout values set.
This is in comparison with a 1ms and 0ms `timer`.

Note how its frequency was limited to 100 times per second in v1.32.x.

| Trigger or Timer \ Tick count  |   ROC 1.0 | Reforged 1.32.10 |
|--------------------------------|----------:|-----------------:|
| 1000ms Trigger periodic        |      1 Hz |             1 Hz |
| 100ms Trigger periodic         |     10 Hz |            10 Hz |
| 20ms Trigger periodic          |     50 Hz |            50 Hz |
| 10ms Trigger periodic          |    100 Hz |           100 Hz |
| 5ms Trigger periodic           |    200 Hz |           100 Hz |
| 1ms Trigger periodic           |   1000 Hz |           100 Hz |
| 0ms Trigger periodic           |  10077 Hz |           100 Hz |
| 1ms Timer                      |   1000 Hz |          1000 Hz |
| 0ms Timer                      |  10077 Hz |         10077 Hz |

See: `TimerStart`
*/
native TriggerRegisterTimerEvent takes trigger whichTrigger, real timeout, boolean periodic returns event


/**
Attach trigger to timer t. The trigger executes each time when timer expires.
Usually used on periodic timers.

Returns event, which is not used by GUI functions.

@note See: `GetExpiredTimer` to retrieve timer inside trigger's actions.
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


/**
Registers to execute whichTrigger when a game event occurs.
Returns a handle to event that represents the registration, you can't do anything with those currently.

**Example (Lua):**

    trg_gameev = CreateTrigger()
    -- this will print a message when someone opens a build menu
    TriggerAddAction(trg_gameev, function() print(GetTriggerEventId()) end)
    TriggerRegisterGameEvent(trg_gameev, EVENT_GAME_BUILD_SUBMENU)
    --> new event on build menu open

@bug Registered events cannot be destroyed as an object.
*/
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



/**
Registers when a player clicks on the given `trackable`.
*/
native TriggerRegisterTrackableHitEvent takes trigger whichTrigger, trackable t returns event

/**
Registers when a player hovers over the given `trackable`.
*/
native TriggerRegisterTrackableTrackEvent takes trigger whichTrigger, trackable t returns event


/**
@event `EVENT_GAME_TRACKABLE_HIT`
@event `EVENT_GAME_TRACKABLE_TRACK`
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

/**
@patch 1.32
@event EVENT_COMMAND_BUTTON_CLICK
*/
native TriggerRegisterCommandEvent takes trigger whichTrigger, integer whichAbility, string order returns event

/**
@patch 1.32
@event EVENT_COMMAND_BUTTON_CLICK
*/
native TriggerRegisterUpgradeCommandEvent takes trigger whichTrigger, integer whichUpgrade returns event
