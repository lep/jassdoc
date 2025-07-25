//===========================================================================
// Blizzard.j ( define Jass2 functions that need to be in every map script )
//===========================================================================


globals
    //-----------------------------------------------------------------------
    // Constants
    //

    // Misc constants

/**
@patch 1.00
*/
    constant real      bj_PI                            = 3.14159

/**
@patch 1.00
*/
    constant real      bj_E                             = 2.71828

/**
@patch 1.00
*/
    constant real      bj_CELLWIDTH                     = 128.0

/**
@patch 1.07
*/
    constant real      bj_CLIFFHEIGHT                   = 128.0

/**
Specifies the default unit rotation for BJ functions. Set to 270° meaning facing south.

* 0   = East
* 90  = North
* 180 = West
* 270 = South
* -90 = South (wraps around)

@patch 1.00
*/
    constant real      bj_UNIT_FACING                   = 270.0

/**
@patch 1.00
*/
    constant real      bj_RADTODEG                      = 180.0/bj_PI

/**
@patch 1.00
*/
    constant real      bj_DEGTORAD                      = bj_PI/180.0

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_QUEST              = 20.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_QUESTUPDATE        = 20.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_QUESTDONE          = 20.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_QUESTFAILED        = 20.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_QUESTREQUIREMENT   = 20.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_MISSIONFAILED      = 20.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_ALWAYSHINT         = 12.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_HINT               = 12.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_SECRET             = 10.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_UNITACQUIRED       = 15.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_UNITAVAILABLE      = 10.00

/**
@patch 1.00
*/
    constant real      bj_TEXT_DELAY_ITEMACQUIRED       = 10.00

/**
@patch 1.07
*/
    constant real      bj_TEXT_DELAY_WARNING            = 12.00

/**
@patch 1.00
*/
    constant real      bj_QUEUE_DELAY_QUEST             =  5.00

/**
@patch 1.00
*/
    constant real      bj_QUEUE_DELAY_HINT              =  5.00

/**
@patch 1.00
*/
    constant real      bj_QUEUE_DELAY_SECRET            =  3.00

/**
@patch 1.00
*/
    constant real      bj_HANDICAP_EASY                 = 60.00

/**
@patch 1.32.0.14411
*/
    constant real      bj_HANDICAP_NORMAL               = 90.00

/**
@patch 1.32.0.13369
*/
    constant real      bj_HANDICAPDAMAGE_EASY           = 50.00

/**
@patch 1.32.0.14411
*/
    constant real      bj_HANDICAPDAMAGE_NORMAL         = 90.00

/**
@patch 1.32.0.13369
*/
	constant real      bj_HANDICAPREVIVE_NOTHARD        = 50.00

/**
@patch 1.00
*/
    constant real      bj_GAME_STARTED_THRESHOLD        =  0.01

/**
@patch 1.07
*/
    constant real      bj_WAIT_FOR_COND_MIN_INTERVAL    =  0.10

/**
@patch 1.07
*/
    constant real      bj_POLLED_WAIT_INTERVAL          =  0.10

/**
@patch 1.07
*/
    constant real      bj_POLLED_WAIT_SKIP_THRESHOLD    =  2.00

    // Game constants

/**
@patch 1.00
*/
    constant integer   bj_MAX_INVENTORY                 =  6

/**
Stores the maximum number of playable player slots regardless of map options.

@note See: `GetBJMaxPlayers`, `GetBJMaxPlayerSlots`

@patch 1.00
*/
    constant integer   bj_MAX_PLAYERS                   =  GetBJMaxPlayers()

/**
Stores the zero-based ID of neutral victim player.

@note See: `GetBJPlayerNeutralVictim`, `GetPlayerNeutralAggressive`, `GetBJPlayerNeutralExtra`, `GetPlayerNeutralPassive`

@patch 1.00
*/
    constant integer   bj_PLAYER_NEUTRAL_VICTIM         =  GetBJPlayerNeutralVictim()

/**
Stores the zero-based ID of neutral extra player.

@note See: `GetBJPlayerNeutralExtra`, `GetPlayerNeutralAggressive`, `GetPlayerNeutralPassive`, `GetBJPlayerNeutralVictim`

@patch 1.07
*/
    constant integer   bj_PLAYER_NEUTRAL_EXTRA          =  GetBJPlayerNeutralExtra()

/**
Stores the maximum number of internal player slots regardless of map options.

@note See: `GetBJMaxPlayerSlots`, `GetBJMaxPlayers`

@patch 1.00
*/
    constant integer   bj_MAX_PLAYER_SLOTS              =  GetBJMaxPlayerSlots()

/**
@patch 1.00
*/
    constant integer   bj_MAX_SKELETONS                 =  25

/**
@patch 1.07
*/
    constant integer   bj_MAX_STOCK_ITEM_SLOTS          =  11

/**
@patch 1.07
*/
    constant integer   bj_MAX_STOCK_UNIT_SLOTS          =  11

/**
@patch 1.07
*/
    constant integer   bj_MAX_ITEM_LEVEL                =  10
    
    // Auto Save constants

/**
@patch 1.32.0.13369
*/
    constant integer   bj_MAX_CHECKPOINTS               =  5

    // Ideally these would be looked up from Units/MiscData.txt,
    // but there is currently no script functionality exposed to do that

/**
@patch 1.00
*/
    constant real      bj_TOD_DAWN                      = 6.00

/**
@patch 1.00
*/
    constant real      bj_TOD_DUSK                      = 18.00

    // Melee game settings:
    //   - Starting Time of Day (TOD)
    //   - Starting Gold
    //   - Starting Lumber
    //   - Starting Hero Tokens (free heroes)
    //   - Max heroes allowed per player
    //   - Max heroes allowed per hero type
    //   - Distance from start loc to search for nearby mines
    //

/**
@patch 1.00
*/
    constant real      bj_MELEE_STARTING_TOD            = 8.00

/**
@patch 1.07
*/
    constant integer   bj_MELEE_STARTING_GOLD_V0        = 750

/**
@patch 1.07
*/
    constant integer   bj_MELEE_STARTING_GOLD_V1        = 500

/**
@patch 1.07
*/
    constant integer   bj_MELEE_STARTING_LUMBER_V0      = 200

/**
@patch 1.07
*/
    constant integer   bj_MELEE_STARTING_LUMBER_V1      = 150

/**
@patch 1.00
*/
    constant integer   bj_MELEE_STARTING_HERO_TOKENS    = 1

/**
@patch 1.00
*/
    constant integer   bj_MELEE_HERO_LIMIT              = 3

/**
@patch 1.00
*/
    constant integer   bj_MELEE_HERO_TYPE_LIMIT         = 1

/**
@patch 1.00
*/
    constant real      bj_MELEE_MINE_SEARCH_RADIUS      = 2000

/**
@patch 1.07
*/
    constant real      bj_MELEE_CLEAR_UNITS_RADIUS      = 1500

/**
@patch 1.07
*/
    constant real      bj_MELEE_CRIPPLE_TIMEOUT         = 120.00

/**
@patch 1.07
*/
    constant real      bj_MELEE_CRIPPLE_MSG_DURATION    = 20.00

/**
@patch 1.07
*/
    constant integer   bj_MELEE_MAX_TWINKED_HEROES_V0   = 3

/**
@patch 1.07
*/
    constant integer   bj_MELEE_MAX_TWINKED_HEROES_V1   = 1

    // Delay between a creep's death and the time it may drop an item.

/**
@patch 1.00
*/
    constant real      bj_CREEP_ITEM_DELAY              = 0.50

    // Timing settings for Marketplace inventories.

/**
@patch 1.07
*/
    constant real      bj_STOCK_RESTOCK_INITIAL_DELAY   = 120

/**
@patch 1.07
*/
    constant real      bj_STOCK_RESTOCK_INTERVAL        = 30

/**
@patch 1.07
*/
    constant integer   bj_STOCK_MAX_ITERATIONS          = 20

    // Max events registered by a single "dest dies in region" event.

/**
@patch 1.07
*/
    constant integer   bj_MAX_DEST_IN_REGION_EVENTS     = 64

    // Camera settings

/**
@patch 1.00
*/
    constant integer   bj_CAMERA_MIN_FARZ               = 100

/**
@patch 1.00
*/
    constant integer   bj_CAMERA_DEFAULT_DISTANCE       = 1650

/**
@patch 1.00
*/
    constant integer   bj_CAMERA_DEFAULT_FARZ           = 5000

/**
@patch 1.00
*/
    constant integer   bj_CAMERA_DEFAULT_AOA            = 304

/**
@patch 1.00
*/
    constant integer   bj_CAMERA_DEFAULT_FOV            = 70

/**
@patch 1.00
*/
    constant integer   bj_CAMERA_DEFAULT_ROLL           = 0

/**
@patch 1.00
*/
    constant integer   bj_CAMERA_DEFAULT_ROTATION       = 90

    // Rescue

/**
@patch 1.00
*/
    constant real      bj_RESCUE_PING_TIME              = 2.00

    // Transmission behavior settings

/**
@patch 1.00
*/
    constant real      bj_NOTHING_SOUND_DURATION        = 5.00

/**
@patch 1.00
*/
    constant real      bj_TRANSMISSION_PING_TIME        = 1.00

/**
@patch 1.00
*/
    constant integer   bj_TRANSMISSION_IND_RED          = 255

/**
@patch 1.00
*/
    constant integer   bj_TRANSMISSION_IND_BLUE         = 255

/**
@patch 1.00
*/
    constant integer   bj_TRANSMISSION_IND_GREEN        = 255

/**
@patch 1.00
*/
    constant integer   bj_TRANSMISSION_IND_ALPHA        = 255

/**
@patch 1.00
*/
    constant real      bj_TRANSMISSION_PORT_HANGTIME    = 1.50

    // Cinematic mode settings

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_INTERFACEFADE        = 0.50

/**
@patch 1.00
*/
    constant gamespeed bj_CINEMODE_GAMESPEED            = MAP_SPEED_NORMAL

    // Cinematic mode volume levels

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_UNITMOVEMENT  = 0.40

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_UNITSOUNDS    = 0.00

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_COMBAT        = 0.40

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_SPELLS        = 0.40

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_UI            = 0.00

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_MUSIC         = 0.55

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_AMBIENTSOUNDS = 1.00

/**
@patch 1.00
*/
    constant real      bj_CINEMODE_VOLUME_FIRE          = 0.60

    // Speech mode volume levels

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_UNITMOVEMENT    = 0.25

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_UNITSOUNDS      = 0.00

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_COMBAT          = 0.25

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_SPELLS          = 0.25

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_UI              = 0.00

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_MUSIC           = 0.55

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_AMBIENTSOUNDS   = 1.00

/**
@patch 1.00
*/
    constant real      bj_SPEECH_VOLUME_FIRE            = 0.60

    // Smart pan settings

/**
@patch 1.00
*/
    constant real      bj_SMARTPAN_TRESHOLD_PAN         = 500

/**
@patch 1.00
*/
    constant real      bj_SMARTPAN_TRESHOLD_SNAP        = 3500

    // QueuedTriggerExecute settings

/**
@patch 1.00
*/
    constant integer   bj_MAX_QUEUED_TRIGGERS           = 100

/**
@patch 1.00
*/
    constant real      bj_QUEUED_TRIGGER_TIMEOUT        = 180.00

    // Campaign indexing constants

/**
@patch 1.00
*/
    constant integer   bj_CAMPAIGN_INDEX_T        = 0

/**
@patch 1.00
*/
    constant integer   bj_CAMPAIGN_INDEX_H        = 1

/**
@patch 1.00
*/
    constant integer   bj_CAMPAIGN_INDEX_U        = 2

/**
@patch 1.00
*/
    constant integer   bj_CAMPAIGN_INDEX_O        = 3

/**
@patch 1.00
*/
    constant integer   bj_CAMPAIGN_INDEX_N        = 4

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_INDEX_XN       = 5

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_INDEX_XH       = 6

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_INDEX_XU       = 7

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_INDEX_XO       = 8

    // Campaign offset constants (for mission indexing)

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_T       = 0

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_H       = 1

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_U       = 2

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_O       = 3

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_N       = 4

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_XN      = 5

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_XH      = 6

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_XU      = 7

/**
@patch 1.07
*/
    constant integer   bj_CAMPAIGN_OFFSET_XO      = 8

    // Mission indexing constants
    // Tutorial

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_T00       = bj_CAMPAIGN_OFFSET_T * 1000 + 0

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_T01       = bj_CAMPAIGN_OFFSET_T * 1000 + 1

/**
@patch 1.32.0.13369
*/
    constant integer   bj_MISSION_INDEX_T02       = bj_CAMPAIGN_OFFSET_T * 1000 + 2

/**
@patch 1.32.0.13369
*/
    constant integer   bj_MISSION_INDEX_T03       = bj_CAMPAIGN_OFFSET_T * 1000 + 3

/**
@patch 1.32.0.13369
*/
    constant integer   bj_MISSION_INDEX_T04       = bj_CAMPAIGN_OFFSET_T * 1000 + 4
    // Human

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H00       = bj_CAMPAIGN_OFFSET_H * 1000 + 0

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H01       = bj_CAMPAIGN_OFFSET_H * 1000 + 1

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H02       = bj_CAMPAIGN_OFFSET_H * 1000 + 2

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H03       = bj_CAMPAIGN_OFFSET_H * 1000 + 3

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H04       = bj_CAMPAIGN_OFFSET_H * 1000 + 4

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H05       = bj_CAMPAIGN_OFFSET_H * 1000 + 5

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H06       = bj_CAMPAIGN_OFFSET_H * 1000 + 6

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H07       = bj_CAMPAIGN_OFFSET_H * 1000 + 7

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H08       = bj_CAMPAIGN_OFFSET_H * 1000 + 8

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H09       = bj_CAMPAIGN_OFFSET_H * 1000 + 9

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H10       = bj_CAMPAIGN_OFFSET_H * 1000 + 10

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_H11       = bj_CAMPAIGN_OFFSET_H * 1000 + 11
    // Undead

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U00       = bj_CAMPAIGN_OFFSET_U * 1000 + 0

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U01       = bj_CAMPAIGN_OFFSET_U * 1000 + 1

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U02       = bj_CAMPAIGN_OFFSET_U * 1000 + 2

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U03       = bj_CAMPAIGN_OFFSET_U * 1000 + 3

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U05       = bj_CAMPAIGN_OFFSET_U * 1000 + 4

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U07       = bj_CAMPAIGN_OFFSET_U * 1000 + 5

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U08       = bj_CAMPAIGN_OFFSET_U * 1000 + 6

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U09       = bj_CAMPAIGN_OFFSET_U * 1000 + 7

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U10       = bj_CAMPAIGN_OFFSET_U * 1000 + 8

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_U11       = bj_CAMPAIGN_OFFSET_U * 1000 + 9
    // Orc

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O00       = bj_CAMPAIGN_OFFSET_O * 1000 + 0

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O01       = bj_CAMPAIGN_OFFSET_O * 1000 + 1

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O02       = bj_CAMPAIGN_OFFSET_O * 1000 + 2

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O03       = bj_CAMPAIGN_OFFSET_O * 1000 + 3

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O04       = bj_CAMPAIGN_OFFSET_O * 1000 + 4

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O05       = bj_CAMPAIGN_OFFSET_O * 1000 + 5

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O06       = bj_CAMPAIGN_OFFSET_O * 1000 + 6

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O07       = bj_CAMPAIGN_OFFSET_O * 1000 + 7

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O08       = bj_CAMPAIGN_OFFSET_O * 1000 + 8

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O09       = bj_CAMPAIGN_OFFSET_O * 1000 + 9

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_O10       = bj_CAMPAIGN_OFFSET_O * 1000 + 10
    // Night Elf

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N00       = bj_CAMPAIGN_OFFSET_N * 1000 + 0

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N01       = bj_CAMPAIGN_OFFSET_N * 1000 + 1

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N02       = bj_CAMPAIGN_OFFSET_N * 1000 + 2

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N03       = bj_CAMPAIGN_OFFSET_N * 1000 + 3

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N04       = bj_CAMPAIGN_OFFSET_N * 1000 + 4

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N05       = bj_CAMPAIGN_OFFSET_N * 1000 + 5

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N06       = bj_CAMPAIGN_OFFSET_N * 1000 + 6

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N07       = bj_CAMPAIGN_OFFSET_N * 1000 + 7

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N08       = bj_CAMPAIGN_OFFSET_N * 1000 + 8

/**
@patch 1.00
*/
    constant integer   bj_MISSION_INDEX_N09       = bj_CAMPAIGN_OFFSET_N * 1000 + 9
    // Expansion Night Elf

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN00       = bj_CAMPAIGN_OFFSET_XN * 1000 + 0

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN01       = bj_CAMPAIGN_OFFSET_XN * 1000 + 1

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN02       = bj_CAMPAIGN_OFFSET_XN * 1000 + 2

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN03       = bj_CAMPAIGN_OFFSET_XN * 1000 + 3

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN04       = bj_CAMPAIGN_OFFSET_XN * 1000 + 4

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN05       = bj_CAMPAIGN_OFFSET_XN * 1000 + 5

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN06       = bj_CAMPAIGN_OFFSET_XN * 1000 + 6

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN07       = bj_CAMPAIGN_OFFSET_XN * 1000 + 7

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN08       = bj_CAMPAIGN_OFFSET_XN * 1000 + 8

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN09       = bj_CAMPAIGN_OFFSET_XN * 1000 + 9

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XN10       = bj_CAMPAIGN_OFFSET_XN * 1000 + 10
    // Expansion Human

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH00       = bj_CAMPAIGN_OFFSET_XH * 1000 + 0

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH01       = bj_CAMPAIGN_OFFSET_XH * 1000 + 1

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH02       = bj_CAMPAIGN_OFFSET_XH * 1000 + 2

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH03       = bj_CAMPAIGN_OFFSET_XH * 1000 + 3

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH04       = bj_CAMPAIGN_OFFSET_XH * 1000 + 4

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH05       = bj_CAMPAIGN_OFFSET_XH * 1000 + 5

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH06       = bj_CAMPAIGN_OFFSET_XH * 1000 + 6

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH07       = bj_CAMPAIGN_OFFSET_XH * 1000 + 7

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH08       = bj_CAMPAIGN_OFFSET_XH * 1000 + 8

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XH09       = bj_CAMPAIGN_OFFSET_XH * 1000 + 9
    // Expansion Undead

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU00       = bj_CAMPAIGN_OFFSET_XU * 1000 + 0

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU01       = bj_CAMPAIGN_OFFSET_XU * 1000 + 1

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU02       = bj_CAMPAIGN_OFFSET_XU * 1000 + 2

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU03       = bj_CAMPAIGN_OFFSET_XU * 1000 + 3

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU04       = bj_CAMPAIGN_OFFSET_XU * 1000 + 4

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU05       = bj_CAMPAIGN_OFFSET_XU * 1000 + 5

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU06       = bj_CAMPAIGN_OFFSET_XU * 1000 + 6

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU07       = bj_CAMPAIGN_OFFSET_XU * 1000 + 7

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU08       = bj_CAMPAIGN_OFFSET_XU * 1000 + 8

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU09       = bj_CAMPAIGN_OFFSET_XU * 1000 + 9

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU10       = bj_CAMPAIGN_OFFSET_XU * 1000 + 10

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU11       = bj_CAMPAIGN_OFFSET_XU * 1000 + 11

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU12       = bj_CAMPAIGN_OFFSET_XU * 1000 + 12

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XU13       = bj_CAMPAIGN_OFFSET_XU * 1000 + 13

    // Expansion Orc

/**
@patch 1.07
*/
    constant integer   bj_MISSION_INDEX_XO00       = bj_CAMPAIGN_OFFSET_XO * 1000 + 0

/**
@patch 1.30.0.9655
*/
    constant integer   bj_MISSION_INDEX_XO01       = bj_CAMPAIGN_OFFSET_XO * 1000 + 1

/**
@patch 1.30.0.9655
*/
    constant integer   bj_MISSION_INDEX_XO02       = bj_CAMPAIGN_OFFSET_XO * 1000 + 2

/**
@patch 1.30.0.9655
*/
    constant integer   bj_MISSION_INDEX_XO03       = bj_CAMPAIGN_OFFSET_XO * 1000 + 3

    // Cinematic indexing constants

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_TOP      = 0

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_HOP      = 1

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_HED      = 2

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_OOP      = 3

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_OED      = 4

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_UOP      = 5

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_UED      = 6

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_NOP      = 7

/**
@patch 1.00
*/
    constant integer   bj_CINEMATICINDEX_NED      = 8

/**
@patch 1.07
*/
    constant integer   bj_CINEMATICINDEX_XOP      = 9

/**
@patch 1.07
*/
    constant integer   bj_CINEMATICINDEX_XED      = 10

    // Alliance settings

/**
@patch 1.00
*/
    constant integer   bj_ALLIANCE_UNALLIED        = 0

/**
@patch 1.00
*/
    constant integer   bj_ALLIANCE_UNALLIED_VISION = 1

/**
@patch 1.00
*/
    constant integer   bj_ALLIANCE_ALLIED          = 2

/**
@patch 1.00
*/
    constant integer   bj_ALLIANCE_ALLIED_VISION   = 3

/**
@patch 1.00
*/
    constant integer   bj_ALLIANCE_ALLIED_UNITS    = 4

/**
@patch 1.00
*/
    constant integer   bj_ALLIANCE_ALLIED_ADVUNITS = 5

/**
@patch 1.00
*/
    constant integer   bj_ALLIANCE_NEUTRAL         = 6

/**
@patch 1.07
*/
    constant integer   bj_ALLIANCE_NEUTRAL_VISION  = 7

    // Keyboard Event Types

/**
@patch 1.07
*/
    constant integer   bj_KEYEVENTTYPE_DEPRESS     = 0

/**
@patch 1.07
*/
    constant integer   bj_KEYEVENTTYPE_RELEASE     = 1

    // Keyboard Event Keys

/**
@patch 1.07
*/
    constant integer   bj_KEYEVENTKEY_LEFT         = 0

/**
@patch 1.07
*/
    constant integer   bj_KEYEVENTKEY_RIGHT        = 1

/**
@patch 1.07
*/
    constant integer   bj_KEYEVENTKEY_DOWN         = 2

/**
@patch 1.07
*/
    constant integer   bj_KEYEVENTKEY_UP           = 3

    // Mouse Event Types

/**
@patch 1.29.0.8803
*/
    constant integer   bj_MOUSEEVENTTYPE_DOWN     = 0

/**
@patch 1.29.0.8803
*/
    constant integer   bj_MOUSEEVENTTYPE_UP       = 1

/**
@patch 1.29.0.8803
*/
    constant integer   bj_MOUSEEVENTTYPE_MOVE     = 2

    // Transmission timing methods

/**
@patch 1.00
*/
    constant integer   bj_TIMETYPE_ADD             = 0

/**
@patch 1.00
*/
    constant integer   bj_TIMETYPE_SET             = 1

/**
@patch 1.00
*/
    constant integer   bj_TIMETYPE_SUB             = 2

    // Camera bounds adjustment methods

/**
@patch 1.00
*/
    constant integer   bj_CAMERABOUNDS_ADJUST_ADD  = 0

/**
@patch 1.00
*/
    constant integer   bj_CAMERABOUNDS_ADJUST_SUB  = 1

    // Quest creation states

/**
@patch 1.00
*/
    constant integer   bj_QUESTTYPE_REQ_DISCOVERED   = 0

/**
@patch 1.00
*/
    constant integer   bj_QUESTTYPE_REQ_UNDISCOVERED = 1

/**
@patch 1.00
*/
    constant integer   bj_QUESTTYPE_OPT_DISCOVERED   = 2

/**
@patch 1.00
*/
    constant integer   bj_QUESTTYPE_OPT_UNDISCOVERED = 3

    // Quest message types

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_DISCOVERED    = 0

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_UPDATED       = 1

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_COMPLETED     = 2

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_FAILED        = 3

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_REQUIREMENT   = 4

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_MISSIONFAILED = 5

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_ALWAYSHINT    = 6

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_HINT          = 7

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_SECRET        = 8

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_UNITACQUIRED  = 9

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_UNITAVAILABLE = 10

/**
@patch 1.00
*/
    constant integer   bj_QUESTMESSAGE_ITEMACQUIRED  = 11

/**
@patch 1.07
*/
    constant integer   bj_QUESTMESSAGE_WARNING       = 12

    // Leaderboard sorting methods

/**
@patch 1.00
*/
    constant integer   bj_SORTTYPE_SORTBYVALUE     = 0

/**
@patch 1.00
*/
    constant integer   bj_SORTTYPE_SORTBYPLAYER    = 1

/**
@patch 1.00
*/
    constant integer   bj_SORTTYPE_SORTBYLABEL     = 2

    // Cinematic fade filter methods

/**
@patch 1.00
*/
    constant integer   bj_CINEFADETYPE_FADEIN      = 0

/**
@patch 1.00
*/
    constant integer   bj_CINEFADETYPE_FADEOUT     = 1

/**
@patch 1.00
*/
    constant integer   bj_CINEFADETYPE_FADEOUTIN   = 2

    // Buff removal methods

/**
@patch 1.00
*/
    constant integer   bj_REMOVEBUFFS_POSITIVE     = 0

/**
@patch 1.00
*/
    constant integer   bj_REMOVEBUFFS_NEGATIVE     = 1

/**
@patch 1.00
*/
    constant integer   bj_REMOVEBUFFS_ALL          = 2

/**
@patch 1.07
*/
    constant integer   bj_REMOVEBUFFS_NONTLIFE     = 3

    // Buff properties - polarity

/**
@patch 1.07
*/
    constant integer   bj_BUFF_POLARITY_POSITIVE   = 0

/**
@patch 1.07
*/
    constant integer   bj_BUFF_POLARITY_NEGATIVE   = 1

/**
@patch 1.07
*/
    constant integer   bj_BUFF_POLARITY_EITHER     = 2

    // Buff properties - resist type

/**
@patch 1.07
*/
    constant integer   bj_BUFF_RESIST_MAGIC        = 0

/**
@patch 1.07
*/
    constant integer   bj_BUFF_RESIST_PHYSICAL     = 1

/**
@patch 1.07
*/
    constant integer   bj_BUFF_RESIST_EITHER       = 2

/**
@patch 1.07
*/
    constant integer   bj_BUFF_RESIST_BOTH         = 3

    // Hero stats

/**
@patch 1.07
*/
    constant integer   bj_HEROSTAT_STR             = 0

/**
@patch 1.07
*/
    constant integer   bj_HEROSTAT_AGI             = 1

/**
@patch 1.07
*/
    constant integer   bj_HEROSTAT_INT             = 2

    // Hero skill point modification methods

/**
@patch 1.07
*/
    constant integer   bj_MODIFYMETHOD_ADD    = 0

/**
@patch 1.07
*/
    constant integer   bj_MODIFYMETHOD_SUB    = 1

/**
@patch 1.07
*/
    constant integer   bj_MODIFYMETHOD_SET    = 2

    // Unit state adjustment methods (for replaced units)

/**
@patch 1.00
*/
    constant integer   bj_UNIT_STATE_METHOD_ABSOLUTE = 0

/**
@patch 1.00
*/
    constant integer   bj_UNIT_STATE_METHOD_RELATIVE = 1

/**
@patch 1.00
*/
    constant integer   bj_UNIT_STATE_METHOD_DEFAULTS = 2

/**
@patch 1.00
*/
    constant integer   bj_UNIT_STATE_METHOD_MAXIMUM  = 3

    // Gate operations

/**
@patch 1.00
*/
    constant integer   bj_GATEOPERATION_CLOSE      = 0

/**
@patch 1.00
*/
    constant integer   bj_GATEOPERATION_OPEN       = 1

/**
@patch 1.00
*/
    constant integer   bj_GATEOPERATION_DESTROY    = 2

	// Game cache value types

/**
@patch 1.07
*/
	constant integer   bj_GAMECACHE_BOOLEAN                 = 0

/**
@patch 1.07
*/
	constant integer   bj_GAMECACHE_INTEGER                 = 1

/**
@patch 1.07
*/
	constant integer   bj_GAMECACHE_REAL                    = 2

/**
@patch 1.07
*/
	constant integer   bj_GAMECACHE_UNIT                    = 3

/**
@patch 1.07
*/
	constant integer   bj_GAMECACHE_STRING                  = 4
	
	// Hashtable value types

/**
@patch 1.24a
*/
	constant integer   bj_HASHTABLE_BOOLEAN                 = 0

/**
@patch 1.24a
*/
	constant integer   bj_HASHTABLE_INTEGER                 = 1

/**
@patch 1.24a
*/
	constant integer   bj_HASHTABLE_REAL                    = 2

/**
@patch 1.24a
*/
	constant integer   bj_HASHTABLE_STRING                  = 3

/**
@patch 1.24a
*/
	constant integer   bj_HASHTABLE_HANDLE                  = 4

    // Item status types

/**
@patch 1.07
*/
    constant integer   bj_ITEM_STATUS_HIDDEN       = 0

/**
@patch 1.07
*/
    constant integer   bj_ITEM_STATUS_OWNED        = 1

/**
@patch 1.07
*/
    constant integer   bj_ITEM_STATUS_INVULNERABLE = 2

/**
@patch 1.07
*/
    constant integer   bj_ITEM_STATUS_POWERUP      = 3

/**
@patch 1.07
*/
    constant integer   bj_ITEM_STATUS_SELLABLE     = 4

/**
@patch 1.07
*/
    constant integer   bj_ITEM_STATUS_PAWNABLE     = 5

    // Itemcode status types

/**
@patch 1.07
*/
    constant integer   bj_ITEMCODE_STATUS_POWERUP  = 0

/**
@patch 1.07
*/
    constant integer   bj_ITEMCODE_STATUS_SELLABLE = 1

/**
@patch 1.07
*/
    constant integer   bj_ITEMCODE_STATUS_PAWNABLE = 2

    // Minimap ping styles

/**
@patch 1.07
*/
    constant integer   bj_MINIMAPPINGSTYLE_SIMPLE  = 0

/**
@patch 1.07
*/
    constant integer   bj_MINIMAPPINGSTYLE_FLASHY  = 1

/**
@patch 1.07
*/
    constant integer   bj_MINIMAPPINGSTYLE_ATTACK  = 2
	
    // Campaign Minimap icon styles

/**
@patch 1.32.0.13369
*/
    constant integer   bj_CAMPPINGSTYLE_PRIMARY			= 0

/**
@patch 1.32.0.14411
*/
    constant integer   bj_CAMPPINGSTYLE_PRIMARY_GREEN   = 1

/**
@patch 1.32.0.14411
*/
    constant integer   bj_CAMPPINGSTYLE_PRIMARY_RED     = 2

/**
@patch 1.32.0.13369
*/
    constant integer   bj_CAMPPINGSTYLE_BONUS			= 3

/**
@patch 1.32.0.13369
*/
    constant integer   bj_CAMPPINGSTYLE_TURNIN			= 4

/**
@patch 1.32.0.13369
*/
	constant integer   bj_CAMPPINGSTYLE_BOSS			= 5

/**
@patch 1.32.0.13369
*/
	constant integer   bj_CAMPPINGSTYLE_CONTROL_ALLY	= 6

/**
@patch 1.32.0.13369
*/
	constant integer   bj_CAMPPINGSTYLE_CONTROL_NEUTRAL	= 7

/**
@patch 1.32.0.13369
*/
	constant integer   bj_CAMPPINGSTYLE_CONTROL_ENEMY	= 8

    // Corpse creation settings

/**
@patch 1.07
*/
    constant real      bj_CORPSE_MAX_DEATH_TIME    = 8.00

    // Corpse creation styles

/**
@patch 1.07
*/
    constant integer   bj_CORPSETYPE_FLESH         = 0

/**
@patch 1.07
*/
    constant integer   bj_CORPSETYPE_BONE          = 1

    // Elevator pathing-blocker destructable code

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_BLOCKER_CODE    = 'DTep'

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_CODE01          = 'DTrf'

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_CODE02          = 'DTrx'

    // Elevator wall codes

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_WALL_TYPE_ALL        = 0

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_WALL_TYPE_EAST       = 1

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_WALL_TYPE_NORTH      = 2

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_WALL_TYPE_SOUTH      = 3

/**
@patch 1.07
*/
    constant integer   bj_ELEVATOR_WALL_TYPE_WEST       = 4

    //-----------------------------------------------------------------------
    // Variables
    //

    // Force predefs

/**
@patch 1.00
*/
    force              bj_FORCE_ALL_PLAYERS        = null

/**
@patch 1.00
*/
    force array        bj_FORCE_PLAYER


/**
@patch 1.07
*/
    integer            bj_MELEE_MAX_TWINKED_HEROES = 0

    // Map area rects

/**
@patch 1.00
*/
    rect               bj_mapInitialPlayableArea   = null

/**
@patch 1.00
*/
    rect               bj_mapInitialCameraBounds   = null

    // Utility function vars

/**
@patch 1.00
*/
    integer            bj_forLoopAIndex            = 0

/**
@patch 1.00
*/
    integer            bj_forLoopBIndex            = 0

/**
@patch 1.00
*/
    integer            bj_forLoopAIndexEnd         = 0

/**
@patch 1.00
*/
    integer            bj_forLoopBIndexEnd         = 0


/**
@patch 1.00
*/
    boolean            bj_slotControlReady         = false

/**
@patch 1.00
*/
    boolean array      bj_slotControlUsed

/**
@patch 1.00
*/
    mapcontrol array   bj_slotControl

    // Game started detection vars

/**
@patch 1.00
*/
    timer              bj_gameStartedTimer         = null

/**
@patch 1.00
*/
    boolean            bj_gameStarted              = false

/**
@patch 1.00
*/
    timer              bj_volumeGroupsTimer        = CreateTimer()

    // Singleplayer check

/**
@patch 1.00
*/
    boolean            bj_isSinglePlayer           = false

    // Day/Night Cycle vars

/**
@patch 1.00
*/
    trigger            bj_dncSoundsDay             = null

/**
@patch 1.00
*/
    trigger            bj_dncSoundsNight           = null

/**
@patch 1.00
*/
    sound              bj_dayAmbientSound          = null

/**
@patch 1.00
*/
    sound              bj_nightAmbientSound        = null

/**
@patch 1.00
*/
    trigger            bj_dncSoundsDawn            = null

/**
@patch 1.00
*/
    trigger            bj_dncSoundsDusk            = null

/**
@patch 1.00
*/
    sound              bj_dawnSound                = null

/**
@patch 1.00
*/
    sound              bj_duskSound                = null

/**
@patch 1.00
*/
    boolean            bj_useDawnDuskSounds        = true

/**
@patch 1.00
*/
    boolean            bj_dncIsDaytime             = false

    // Triggered sounds
    //sound              bj_pingMinimapSound         = null

/**
@patch 1.00
*/
    sound              bj_rescueSound              = null

/**
@patch 1.00
*/
    sound              bj_questDiscoveredSound     = null

/**
@patch 1.00
*/
    sound              bj_questUpdatedSound        = null

/**
@patch 1.00
*/
    sound              bj_questCompletedSound      = null

/**
@patch 1.00
*/
    sound              bj_questFailedSound         = null

/**
@patch 1.00
*/
    sound              bj_questHintSound           = null

/**
@patch 1.00
*/
    sound              bj_questSecretSound         = null

/**
@patch 1.00
*/
    sound              bj_questItemAcquiredSound   = null

/**
@patch 1.07
*/
    sound              bj_questWarningSound        = null

/**
@patch 1.00
*/
    sound              bj_victoryDialogSound       = null

/**
@patch 1.00
*/
    sound              bj_defeatDialogSound        = null

    // Marketplace vars

/**
@patch 1.07
*/
    trigger            bj_stockItemPurchased       = null

/**
@patch 1.07
*/
    timer              bj_stockUpdateTimer         = null

/**
@patch 1.07
*/
    boolean array      bj_stockAllowedPermanent

/**
@patch 1.07
*/
    boolean array      bj_stockAllowedCharged

/**
@patch 1.07
*/
    boolean array      bj_stockAllowedArtifact

/**
@patch 1.07
*/
    integer            bj_stockPickedItemLevel     = 0

/**
@patch 1.07
*/
    itemtype           bj_stockPickedItemType

    // Melee vars

/**
@patch 1.00
*/
    trigger            bj_meleeVisibilityTrained   = null

/**
@patch 1.00
*/
    boolean            bj_meleeVisibilityIsDay     = true

/**
@patch 1.00
*/
    boolean            bj_meleeGrantHeroItems      = false

/**
@patch 1.00
*/
    location           bj_meleeNearestMineToLoc    = null

/**
@patch 1.00
*/
    unit               bj_meleeNearestMine         = null

/**
@patch 1.00
*/
    real               bj_meleeNearestMineDist     = 0.00

/**
@patch 1.00
*/
    boolean            bj_meleeGameOver            = false

/**
@patch 1.00
*/
    boolean array      bj_meleeDefeated

/**
@patch 1.00
*/
    boolean array      bj_meleeVictoried

/**
@patch 1.00
*/
    unit array         bj_ghoul

/**
@patch 1.07
*/
    timer array        bj_crippledTimer

/**
@patch 1.07
*/
    timerdialog array  bj_crippledTimerWindows

/**
@patch 1.07
*/
    boolean array      bj_playerIsCrippled

/**
@patch 1.07
*/
    boolean array      bj_playerIsExposed

/**
@patch 1.07
*/
    boolean            bj_finishSoonAllExposed     = false

/**
@patch 1.07
*/
    timerdialog        bj_finishSoonTimerDialog    = null

/**
@patch 1.07
*/
    integer array      bj_meleeTwinkedHeroes

    // Rescue behavior vars

/**
@patch 1.00
*/
    trigger            bj_rescueUnitBehavior       = null

/**
@patch 1.00
*/
    boolean            bj_rescueChangeColorUnit    = true

/**
@patch 1.00
*/
    boolean            bj_rescueChangeColorBldg    = true

    // Transmission vars

/**
@patch 1.00
*/
    timer              bj_cineSceneEndingTimer     = null

/**
@patch 1.00
*/
    sound              bj_cineSceneLastSound       = null

/**
@patch 1.00
*/
    trigger            bj_cineSceneBeingSkipped    = null

    // Cinematic mode vars

/**
@patch 1.00
*/
    gamespeed          bj_cineModePriorSpeed       = MAP_SPEED_NORMAL

/**
@patch 1.00
*/
    boolean            bj_cineModePriorFogSetting  = false

/**
@patch 1.00
*/
    boolean            bj_cineModePriorMaskSetting = false

/**
@patch 1.00
*/
    boolean            bj_cineModeAlreadyIn        = false

/**
@patch 1.07
*/
    boolean            bj_cineModePriorDawnDusk    = false

/**
@patch 1.07
*/
    integer            bj_cineModeSavedSeed        = 0

    // Cinematic fade vars

/**
@patch 1.00
*/
    timer              bj_cineFadeFinishTimer      = null

/**
@patch 1.00
*/
    timer              bj_cineFadeContinueTimer    = null

/**
@patch 1.00
*/
    real               bj_cineFadeContinueRed      = 0

/**
@patch 1.00
*/
    real               bj_cineFadeContinueGreen    = 0

/**
@patch 1.00
*/
    real               bj_cineFadeContinueBlue     = 0

/**
@patch 1.00
*/
    real               bj_cineFadeContinueTrans    = 0

/**
@patch 1.00
*/
    real               bj_cineFadeContinueDuration = 0

/**
@patch 1.00
*/
    string             bj_cineFadeContinueTex      = ""

    // QueuedTriggerExecute vars

/**
@patch 1.00
*/
    integer            bj_queuedExecTotal          = 0

/**
@patch 1.00
*/
    trigger array      bj_queuedExecTriggers

/**
@patch 1.00
*/
    boolean array      bj_queuedExecUseConds

/**
@patch 1.00
*/
    timer              bj_queuedExecTimeoutTimer   = CreateTimer()

/**
@patch 1.00
*/
    trigger            bj_queuedExecTimeout        = null

    // Helper vars (for Filter and Enum funcs)

/**
@patch 1.07
*/
    integer            bj_destInRegionDiesCount    = 0

/**
@patch 1.07
*/
    trigger            bj_destInRegionDiesTrig     = null

/**
@patch 1.00
*/
    integer            bj_groupCountUnits          = 0

/**
@patch 1.00
*/
    integer            bj_forceCountPlayers        = 0

/**
@patch 1.00
*/
    integer            bj_groupEnumTypeId          = 0

/**
@patch 1.00
*/
    player             bj_groupEnumOwningPlayer    = null

/**
@patch 1.00
*/
    group              bj_groupAddGroupDest        = null

/**
@patch 1.00
*/
    group              bj_groupRemoveGroupDest     = null

/**
@patch 1.00
*/
    integer            bj_groupRandomConsidered    = 0

/**
@patch 1.00
*/
    unit               bj_groupRandomCurrentPick   = null

/**
@patch 1.07
*/
    group              bj_groupLastCreatedDest     = null

/**
@patch 1.07
*/
    group              bj_randomSubGroupGroup      = null

/**
@patch 1.07
*/
    integer            bj_randomSubGroupWant       = 0

/**
@patch 1.07
*/
    integer            bj_randomSubGroupTotal      = 0

/**
@patch 1.07
*/
    real               bj_randomSubGroupChance     = 0

/**
@patch 1.00
*/
    integer            bj_destRandomConsidered     = 0

/**
@patch 1.00
*/
    destructable       bj_destRandomCurrentPick    = null

/**
@patch 1.07
*/
    destructable       bj_elevatorWallBlocker      = null

/**
@patch 1.07
*/
    destructable       bj_elevatorNeighbor         = null

/**
@patch 1.07
*/
    integer            bj_itemRandomConsidered     = 0

/**
@patch 1.07
*/
    item               bj_itemRandomCurrentPick    = null

/**
@patch 1.00
*/
    integer            bj_forceRandomConsidered    = 0

/**
@patch 1.00
*/
    player             bj_forceRandomCurrentPick   = null

/**
@patch 1.00
*/
    unit               bj_makeUnitRescuableUnit    = null

/**
@patch 1.00
*/
    boolean            bj_makeUnitRescuableFlag    = true

/**
@patch 1.00
*/
    boolean            bj_pauseAllUnitsFlag        = true

/**
@patch 1.00
*/
    location           bj_enumDestructableCenter   = null

/**
@patch 1.00
*/
    real               bj_enumDestructableRadius   = 0

/**
@patch 1.00
*/
    playercolor        bj_setPlayerTargetColor     = null

/**
@patch 1.00
*/
    boolean            bj_isUnitGroupDeadResult    = true

/**
@patch 1.00
*/
    boolean            bj_isUnitGroupEmptyResult   = true

/**
@patch 1.00
*/
    boolean            bj_isUnitGroupInRectResult  = true

/**
@patch 1.00
*/
    rect               bj_isUnitGroupInRectRect    = null

/**
@patch 1.00
*/
    boolean            bj_changeLevelShowScores    = false

/**
@patch 1.00
*/
    string             bj_changeLevelMapName       = null

/**
@patch 1.07
*/
    group              bj_suspendDecayFleshGroup   = CreateGroup()

/**
@patch 1.07
*/
    group              bj_suspendDecayBoneGroup    = CreateGroup()

/**
@patch 1.07
*/
    timer              bj_delayedSuspendDecayTimer = CreateTimer()

/**
@patch 1.07
*/
    trigger            bj_delayedSuspendDecayTrig  = null

/**
@patch 1.07
*/
    integer            bj_livingPlayerUnitsTypeId  = 0

/**
@patch 1.07
*/
    widget             bj_lastDyingWidget          = null

    // Random distribution vars

/**
@patch 1.00
*/
    integer            bj_randDistCount            = 0

/**
@patch 1.00
*/
    integer array      bj_randDistID

/**
@patch 1.00
*/
    integer array      bj_randDistChance

    // Last X'd vars

/**
@patch 1.00
*/
    unit               bj_lastCreatedUnit          = null

/**
@patch 1.00
*/
    item               bj_lastCreatedItem          = null

/**
@patch 1.00
*/
    item               bj_lastRemovedItem          = null

/**
@patch 1.00
*/
    unit               bj_lastHauntedGoldMine      = null

/**
@patch 1.00
*/
    destructable       bj_lastCreatedDestructable  = null

/**
@patch 1.00
*/
    group              bj_lastCreatedGroup         = CreateGroup()

/**
@patch 1.00
*/
    fogmodifier        bj_lastCreatedFogModifier   = null

/**
@patch 1.00
*/
    effect             bj_lastCreatedEffect        = null

/**
@patch 1.00
*/
    weathereffect      bj_lastCreatedWeatherEffect = null

/**
@patch 1.07
*/
    terraindeformation bj_lastCreatedTerrainDeformation = null

/**
@patch 1.00
*/
    quest              bj_lastCreatedQuest         = null

/**
@patch 1.00
*/
    questitem          bj_lastCreatedQuestItem     = null

/**
@patch 1.00
*/
    defeatcondition    bj_lastCreatedDefeatCondition = null

/**
@patch 1.00
*/
    timer              bj_lastStartedTimer         = CreateTimer()

/**
@patch 1.00
*/
    timerdialog        bj_lastCreatedTimerDialog   = null

/**
@patch 1.00
*/
    leaderboard        bj_lastCreatedLeaderboard   = null

/**
@patch 1.13
*/
    multiboard         bj_lastCreatedMultiboard    = null

/**
@patch 1.00
*/
    sound              bj_lastPlayedSound          = null

/**
@patch 1.00
*/
    string             bj_lastPlayedMusic          = ""

/**
@patch 1.00
*/
    real               bj_lastTransmissionDuration = 0

/**
@patch 1.00
*/
    gamecache          bj_lastCreatedGameCache     = null

/**
@patch 1.24a
*/
    hashtable          bj_lastCreatedHashtable     = null

/**
@patch 1.00
*/
    unit               bj_lastLoadedUnit           = null

/**
@patch 1.00
*/
    button             bj_lastCreatedButton        = null

/**
@patch 1.00
*/
    unit               bj_lastReplacedUnit         = null

/**
@patch 1.07
*/
    texttag            bj_lastCreatedTextTag       = null

/**
@patch 1.17a
*/
    lightning          bj_lastCreatedLightning     = null

/**
@patch 1.18a
*/
    image              bj_lastCreatedImage         = null

/**
@patch 1.18a
*/
    ubersplat          bj_lastCreatedUbersplat     = null

/**
@patch 1.32.0.13369
*/
    minimapicon        bj_lastCreatedMinimapIcon   = null

/**
@patch 1.32.0.13369
*/
	commandbuttoneffect bj_lastCreatedCommandButtonEffect = null

    // Filter function vars

/**
@patch 1.00
*/
    boolexpr           filterIssueHauntOrderAtLocBJ      = null

/**
@patch 1.00
*/
    boolexpr           filterEnumDestructablesInCircleBJ = null

/**
@patch 1.00
*/
    boolexpr           filterGetUnitsInRectOfPlayer      = null

/**
@patch 1.00
*/
    boolexpr           filterGetUnitsOfTypeIdAll         = null

/**
@patch 1.00
*/
    boolexpr           filterGetUnitsOfPlayerAndTypeId   = null

/**
@patch 1.00
*/
    boolexpr           filterMeleeTrainedUnitIsHeroBJ    = null

/**
@patch 1.07
*/
    boolexpr           filterLivingPlayerUnitsOfTypeId   = null

    // Memory cleanup vars

/**
@patch 1.07
*/
    boolean            bj_wantDestroyGroup         = false

    // Instanced Operation Results

/**
@patch 1.31.0.11889
*/
    boolean            bj_lastInstObjFuncSuccessful = true
endglobals



//***************************************************************************
//*
//*  Debugging Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function BJDebugMsg takes string msg returns nothing
    local integer i = 0
    loop
        call DisplayTimedTextToPlayer(Player(i),0,0,60,msg)
        set i = i + 1
        exitwhen i == bj_MAX_PLAYERS
    endloop
endfunction



//***************************************************************************
//*
//*  Math Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function RMinBJ takes real a, real b returns real
    if (a < b) then
        return a
    else
        return b
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RMaxBJ takes real a, real b returns real
    if (a < b) then
        return b
    else
        return a
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RAbsBJ takes real a returns real
    if (a >= 0) then
        return a
    else
        return -a
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RSignBJ takes real a returns real
    if (a >= 0.0) then
        return 1.0
    else
        return -1.0
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IMinBJ takes integer a, integer b returns integer
    if (a < b) then
        return a
    else
        return b
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IMaxBJ takes integer a, integer b returns integer
    if (a < b) then
        return b
    else
        return a
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IAbsBJ takes integer a returns integer
    if (a >= 0) then
        return a
    else
        return -a
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ISignBJ takes integer a returns integer
    if (a >= 0) then
        return 1
    else
        return -1
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SinBJ takes real degrees returns real
    return Sin(degrees * bj_DEGTORAD)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CosBJ takes real degrees returns real
    return Cos(degrees * bj_DEGTORAD)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TanBJ takes real degrees returns real
    return Tan(degrees * bj_DEGTORAD)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AsinBJ takes real degrees returns real
    return Asin(degrees) * bj_RADTODEG
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AcosBJ takes real degrees returns real
    return Acos(degrees) * bj_RADTODEG
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AtanBJ takes real degrees returns real
    return Atan(degrees) * bj_RADTODEG
endfunction

//===========================================================================

/**
@patch 1.00
*/
function Atan2BJ takes real y, real x returns real
    return Atan2(y, x) * bj_RADTODEG
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AngleBetweenPoints takes location locA, location locB returns real
    return bj_RADTODEG * Atan2(GetLocationY(locB) - GetLocationY(locA), GetLocationX(locB) - GetLocationX(locA))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DistanceBetweenPoints takes location locA, location locB returns real
    local real dx = GetLocationX(locB) - GetLocationX(locA)
    local real dy = GetLocationY(locB) - GetLocationY(locA)
    return SquareRoot(dx * dx + dy * dy)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PolarProjectionBJ takes location source, real dist, real angle returns location
    local real x = GetLocationX(source) + dist * Cos(angle * bj_DEGTORAD)
    local real y = GetLocationY(source) + dist * Sin(angle * bj_DEGTORAD)
    return Location(x, y)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetRandomDirectionDeg takes nothing returns real
    return GetRandomReal(0, 360)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetRandomPercentageBJ takes nothing returns real
    return GetRandomReal(0, 100)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetRandomLocInRect takes rect whichRect returns location
    return Location(GetRandomReal(GetRectMinX(whichRect), GetRectMaxX(whichRect)), GetRandomReal(GetRectMinY(whichRect), GetRectMaxY(whichRect)))
endfunction

//===========================================================================
// Calculate the modulus/remainder of (dividend) divided by (divisor).
// Examples:  18 mod 5 = 3.  15 mod 5 = 0.  -8 mod 5 = 2.
//

/**
Calculate the modulus/remainder of (dividend) divided by (divisor) such that
`(dividend / divisor) * divisor + ModuloInteger(dividend, divisor) == dividend` with `/` rounding towards negative infinity.
Examples:  18 mod 5 = 3.  15 mod 5 = 0.  -8 mod 5 = 2.

@note Use the `%`-operator as it's probably faster but also correct.

@bug The commented law doesn't hold. For example `ModuloInteger(-7, -3) == -4` while
`-7 % -3 == -1`

@patch 1.00
*/
function ModuloInteger takes integer dividend, integer divisor returns integer
    local integer modulus = dividend - (dividend / divisor) * divisor

    // If the dividend was negative, the above modulus calculation will
    // be negative, but within (-divisor..0).  We can add (divisor) to
    // shift this result into the desired range of (0..divisor).
    if (modulus < 0) then
        set modulus = modulus + divisor
    endif

    return modulus
endfunction

//===========================================================================
// Calculate the modulus/remainder of (dividend) divided by (divisor).
// Examples:  13.000 mod 2.500 = 0.500.  -6.000 mod 2.500 = 1.500.
//

/**
@patch 1.00
*/
function ModuloReal takes real dividend, real divisor returns real
    local real modulus = dividend - I2R(R2I(dividend / divisor)) * divisor

    // If the dividend was negative, the above modulus calculation will
    // be negative, but within (-divisor..0).  We can add (divisor) to
    // shift this result into the desired range of (0..divisor).
    if (modulus < 0) then
        set modulus = modulus + divisor
    endif

    return modulus
endfunction

//===========================================================================

/**
@patch 1.00
*/
function OffsetLocation takes location loc, real dx, real dy returns location
    return Location(GetLocationX(loc) + dx, GetLocationY(loc) + dy)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function OffsetRectBJ takes rect r, real dx, real dy returns rect
    return Rect( GetRectMinX(r) + dx, GetRectMinY(r) + dy, GetRectMaxX(r) + dx, GetRectMaxY(r) + dy )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RectFromCenterSizeBJ takes location center, real width, real height returns rect
    local real x = GetLocationX( center )
    local real y = GetLocationY( center )
    return Rect( x - width*0.5, y - height*0.5, x + width*0.5, y + height*0.5 )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RectContainsCoords takes rect r, real x, real y returns boolean
    return (GetRectMinX(r) <= x) and (x <= GetRectMaxX(r)) and (GetRectMinY(r) <= y) and (y <= GetRectMaxY(r))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RectContainsLoc takes rect r, location loc returns boolean
    return RectContainsCoords(r, GetLocationX(loc), GetLocationY(loc))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RectContainsUnit takes rect r, unit whichUnit returns boolean
    return RectContainsCoords(r, GetUnitX(whichUnit), GetUnitY(whichUnit))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RectContainsItem takes item whichItem, rect r returns boolean
    if (whichItem == null) then
        return false
    endif

    if (IsItemOwned(whichItem)) then
        return false
    endif

    return RectContainsCoords(r, GetItemX(whichItem), GetItemY(whichItem))
endfunction



//***************************************************************************
//*
//*  Utility Constructs
//*
//***************************************************************************

//===========================================================================
// Runs the trigger's actions if the trigger's conditions evaluate to true.
//

/**
@patch 1.00
*/
function ConditionalTriggerExecute takes trigger trig returns nothing
    if TriggerEvaluate(trig) then
        call TriggerExecute(trig)
    endif
endfunction

//===========================================================================
// Runs the trigger's actions if the trigger's conditions evaluate to true.
//

/**
@patch 1.00
*/
function TriggerExecuteBJ takes trigger trig, boolean checkConditions returns boolean
    if checkConditions then
        if not (TriggerEvaluate(trig)) then
            return false
        endif
    endif
    call TriggerExecute(trig)
    return true
endfunction

//===========================================================================
// Arranges for a trigger to fire almost immediately, except that the calling
// trigger is not interrupted as is the case with a TriggerExecute call.
// Since the trigger executes normally, its conditions are still evaluated.
//

/**
Adds trigger to execution queue by setting it up with a zero-delay timer,
so it is executed almost immediately. Unlike calling another trigger with
`TriggerExecute`, this does not interrupt the currently running trigger.

@param trig Target trigger to execute

@param checkConditions If `true`, check if trigger conditions are met and only then queue the trigger.
If `false`, ignores conditions and always queues the trigger.

Returns:

- `true`, if trigger will be executed
- `false`, if conditions were not met

@patch 1.00
*/
function PostTriggerExecuteBJ takes trigger trig, boolean checkConditions returns boolean
    if checkConditions then
        if not (TriggerEvaluate(trig)) then
            return false
        endif
    endif
    call TriggerRegisterTimerEvent(trig, 0, false)
    return true
endfunction

//===========================================================================
// Debug - Display the contents of the trigger queue (as either null or "x"
// for each entry).

/**
@patch 1.00
*/
function QueuedTriggerCheck takes nothing returns nothing
    local string s = "TrigQueue Check "
    local integer i

    set i = 0
    loop
        exitwhen i >= bj_queuedExecTotal
        set s = s + "q[" + I2S(i) + "]="
        if (bj_queuedExecTriggers[i] == null) then
            set s = s + "null "
        else
            set s = s + "x "
        endif
        set i = i + 1
    endloop
    set s = s + "(" + I2S(bj_queuedExecTotal) + " total)"
    call DisplayTimedTextToPlayer(GetLocalPlayer(),0,0,600,s)
endfunction

//===========================================================================
// Searches the queue for a given trigger, returning the index of the
// trigger within the queue if it is found, or -1 if it is not found.
//

/**
@patch 1.00
*/
function QueuedTriggerGetIndex takes trigger trig returns integer
    // Determine which, if any, of the queued triggers is being removed.
    local integer index     = 0
    loop
        exitwhen index >= bj_queuedExecTotal
        if (bj_queuedExecTriggers[index] == trig) then
            return index
        endif
        set index = index + 1
    endloop
    return -1
endfunction

//===========================================================================
// Removes a trigger from the trigger queue, shifting other triggers down
// to fill the unused space.  If the currently running trigger is removed
// in this manner, this function does NOT attempt to run the next trigger.
//

/**
@patch 1.00
*/
function QueuedTriggerRemoveByIndex takes integer trigIndex returns boolean
    local integer index

    // If the to-be-removed index is out of range, fail.
    if (trigIndex >= bj_queuedExecTotal) then
        return false
    endif

    // Shift all queue entries down to fill in the gap.
    set bj_queuedExecTotal = bj_queuedExecTotal - 1
    set index = trigIndex
    loop
        exitwhen index >= bj_queuedExecTotal
        set bj_queuedExecTriggers[index] = bj_queuedExecTriggers[index + 1]
        set bj_queuedExecUseConds[index] = bj_queuedExecUseConds[index + 1]
        set index = index + 1
    endloop
    return true
endfunction

//===========================================================================
// Attempt to execute the first trigger in the queue.  If it fails, remove
// it and execute the next one.  Continue this cycle until a trigger runs,
// or until the queue is empty.
//

/**
@patch 1.00
*/
function QueuedTriggerAttemptExec takes nothing returns boolean
    loop
        exitwhen bj_queuedExecTotal == 0

        if TriggerExecuteBJ(bj_queuedExecTriggers[0], bj_queuedExecUseConds[0]) then
            // Timeout the queue if it sits at the front of the queue for too long.
            call TimerStart(bj_queuedExecTimeoutTimer, bj_QUEUED_TRIGGER_TIMEOUT, false, null)
            return true
        endif

        call QueuedTriggerRemoveByIndex(0)
    endloop
    return false
endfunction

//===========================================================================
// Queues a trigger to be executed, assuring that such triggers are not
// executed at the same time.
//

/**
@patch 1.00
*/
function QueuedTriggerAddBJ takes trigger trig, boolean checkConditions returns boolean
    // Make sure our queue isn't full.  If it is, return failure.
    if (bj_queuedExecTotal >= bj_MAX_QUEUED_TRIGGERS) then
        return false
    endif

    // Add the trigger to an array of to-be-executed triggers.
    set bj_queuedExecTriggers[bj_queuedExecTotal] = trig
    set bj_queuedExecUseConds[bj_queuedExecTotal] = checkConditions
    set bj_queuedExecTotal = bj_queuedExecTotal + 1

    // If this is the only trigger in the queue, run it.
    if (bj_queuedExecTotal == 1) then
        call QueuedTriggerAttemptExec()
    endif
    return true
endfunction

//===========================================================================
// Denotes the end of a queued trigger. Be sure to call this only once per
// queued trigger, or risk stepping on the toes of other queued triggers.
//

/**
@patch 1.00
*/
function QueuedTriggerRemoveBJ takes trigger trig returns nothing
    local integer index
    local integer trigIndex
    local boolean trigExecuted

    // Find the trigger's index.
    set trigIndex = QueuedTriggerGetIndex(trig)
    if (trigIndex == -1) then
        return
    endif

    // Shuffle the other trigger entries down to fill in the gap.
    call QueuedTriggerRemoveByIndex(trigIndex)

    // If we just axed the currently running trigger, run the next one.
    if (trigIndex == 0) then
        call PauseTimer(bj_queuedExecTimeoutTimer)
        call QueuedTriggerAttemptExec()
    endif
endfunction

//===========================================================================
// Denotes the end of a queued trigger. Be sure to call this only once per
// queued trigger, lest you step on the toes of other queued triggers.
//

/**
@patch 1.00
*/
function QueuedTriggerDoneBJ takes nothing returns nothing
    local integer index

    // Make sure there's something on the queue to remove.
    if (bj_queuedExecTotal <= 0) then
        return
    endif

    // Remove the currently running trigger from the array.
    call QueuedTriggerRemoveByIndex(0)

    // If other triggers are waiting to run, run one of them.
    call PauseTimer(bj_queuedExecTimeoutTimer)
    call QueuedTriggerAttemptExec()
endfunction

//===========================================================================
// Empty the trigger queue.
//

/**
@patch 1.00
*/
function QueuedTriggerClearBJ takes nothing returns nothing
    call PauseTimer(bj_queuedExecTimeoutTimer)
    set bj_queuedExecTotal = 0
endfunction

//===========================================================================
// Remove all but the currently executing trigger from the trigger queue.
//

/**
@patch 1.00
*/
function QueuedTriggerClearInactiveBJ takes nothing returns nothing
    set bj_queuedExecTotal = IMinBJ(bj_queuedExecTotal, 1)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QueuedTriggerCountBJ takes nothing returns integer
    return bj_queuedExecTotal
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsTriggerQueueEmptyBJ takes nothing returns boolean
    return bj_queuedExecTotal <= 0
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsTriggerQueuedBJ takes trigger trig returns boolean
    return QueuedTriggerGetIndex(trig) != -1
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetForLoopIndexA takes nothing returns integer
    return bj_forLoopAIndex
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetForLoopIndexA takes integer newIndex returns nothing
    set bj_forLoopAIndex = newIndex
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetForLoopIndexB takes nothing returns integer
    return bj_forLoopBIndex
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetForLoopIndexB takes integer newIndex returns nothing
    set bj_forLoopBIndex = newIndex
endfunction

//===========================================================================
// We can't do game-time waits, so this simulates one by starting a timer
// and polling until the timer expires.

/**
@bug Leaks handle `t`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function PolledWait takes real duration returns nothing
    local timer t
    local real  timeRemaining

    if (duration > 0) then
        set t = CreateTimer()
        call TimerStart(t, duration, false, null)
        loop
            set timeRemaining = TimerGetRemaining(t)
            exitwhen timeRemaining <= 0

            // If we have a bit of time left, skip past 10% of the remaining
            // duration instead of checking every interval, to minimize the
            // polling on long waits.
            if (timeRemaining > bj_POLLED_WAIT_SKIP_THRESHOLD) then
                call TriggerSleepAction(0.1 * timeRemaining)
            else
                call TriggerSleepAction(bj_POLLED_WAIT_INTERVAL)
            endif
        endloop
        call DestroyTimer(t)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IntegerTertiaryOp takes boolean flag, integer valueA, integer valueB returns integer
    if flag then
        return valueA
    else
        return valueB
    endif
endfunction


//***************************************************************************
//*
//*  General Utility Functions
//*  These functions exist purely to make the trigger dialogs cleaner and
//*  more comprehensible.
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function DoNothing takes nothing returns nothing
endfunction

//===========================================================================
// This function does nothing.  WorldEdit should should eventually ignore
// CommentString triggers during script generation, but until such a time,
// this function will serve as a stub.
//

/**
@patch 1.00
*/
function CommentString takes string commentString returns nothing
endfunction

//===========================================================================
// This function returns the input string, converting it from the localized text, if necessary
//

/**
@patch 1.07
*/
function StringIdentity takes string theString returns string
    return GetLocalizedString(theString)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetBooleanAnd takes boolean valueA, boolean valueB returns boolean
    return valueA and valueB
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetBooleanOr takes boolean valueA, boolean valueB returns boolean
    return valueA or valueB
endfunction

//===========================================================================
// Converts a percentage (real, 0..100) into a scaled integer (0..max),
// clipping the result to 0..max in case the input is invalid.
//

/**
@patch 1.00
*/
function PercentToInt takes real percentage, integer max returns integer
    local real realpercent = percentage * I2R(max) * 0.01
    local integer result = MathRound(realpercent)

    if (result < 0) then
        set result = 0
    elseif (result > max) then
        set result = max
    endif

    return result
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PercentTo255 takes real percentage returns integer
    return PercentToInt(percentage, 255)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetTimeOfDay takes nothing returns real
    return GetFloatGameState(GAME_STATE_TIME_OF_DAY)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetTimeOfDay takes real whatTime returns nothing
    call SetFloatGameState(GAME_STATE_TIME_OF_DAY, whatTime)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetTimeOfDayScalePercentBJ takes real scalePercent returns nothing
    call SetTimeOfDayScale(scalePercent * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetTimeOfDayScalePercentBJ takes nothing returns real
    return GetTimeOfDayScale() * 100
endfunction

//===========================================================================

/**
@bug Leaks handle `soundHandle`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function PlaySound takes string soundName returns nothing
    local sound soundHandle = CreateSound(soundName, false, false, true, 12700, 12700, "")
    call StartSound(soundHandle)
    call KillSoundWhenDone(soundHandle)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CompareLocationsBJ takes location A, location B returns boolean
    return GetLocationX(A) == GetLocationX(B) and GetLocationY(A) == GetLocationY(B)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CompareRectsBJ takes rect A, rect B returns boolean
    return GetRectMinX(A) == GetRectMinX(B) and GetRectMinY(A) == GetRectMinY(B) and GetRectMaxX(A) == GetRectMaxX(B) and GetRectMaxY(A) == GetRectMaxY(B)
endfunction

//===========================================================================
// Returns a square rect that exactly encompasses the specified circle.
//

/**
Returns a new, centered rectangle with the dimensions to encompass the circle. 

@note It does not touch the passed `center` location, you must remove it manually to avoid leaks.

@patch 1.00
*/
function GetRectFromCircleBJ takes location center, real radius returns rect
    local real centerX = GetLocationX(center)
    local real centerY = GetLocationY(center)
    return Rect(centerX - radius, centerY - radius, centerX + radius, centerY + radius)
endfunction



//***************************************************************************
//*
//*  Camera Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@bug Leaks handle `theCam`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function GetCurrentCameraSetup takes nothing returns camerasetup
    local camerasetup theCam = CreateCameraSetup()
    local real duration = 0
    call CameraSetupSetField(theCam, CAMERA_FIELD_TARGET_DISTANCE, GetCameraField(CAMERA_FIELD_TARGET_DISTANCE), duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_FARZ,            GetCameraField(CAMERA_FIELD_FARZ),            duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_ZOFFSET,         GetCameraField(CAMERA_FIELD_ZOFFSET),         duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_ANGLE_OF_ATTACK, bj_RADTODEG * GetCameraField(CAMERA_FIELD_ANGLE_OF_ATTACK), duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_FIELD_OF_VIEW,   bj_RADTODEG * GetCameraField(CAMERA_FIELD_FIELD_OF_VIEW),   duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_ROLL,            bj_RADTODEG * GetCameraField(CAMERA_FIELD_ROLL),            duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_ROTATION,        bj_RADTODEG * GetCameraField(CAMERA_FIELD_ROTATION),        duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_LOCAL_PITCH,     bj_RADTODEG * GetCameraField(CAMERA_FIELD_LOCAL_PITCH),     duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_LOCAL_YAW,       bj_RADTODEG * GetCameraField(CAMERA_FIELD_LOCAL_YAW),       duration)
    call CameraSetupSetField(theCam, CAMERA_FIELD_LOCAL_ROLL,      bj_RADTODEG * GetCameraField(CAMERA_FIELD_LOCAL_ROLL),      duration)
    call CameraSetupSetDestPosition(theCam, GetCameraTargetPositionX(), GetCameraTargetPositionY(), duration)
    return theCam
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CameraSetupApplyForPlayer takes boolean doPan, camerasetup whichSetup, player whichPlayer, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call CameraSetupApplyForceDuration(whichSetup, doPan, duration)
    endif
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function CameraSetupApplyForPlayerSmooth takes boolean doPan, camerasetup whichSetup, player whichPlayer, real forcedDuration, real easeInDuration, real easeOutDuration, real smoothFactor returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call BlzCameraSetupApplyForceDurationSmooth(whichSetup, doPan, forcedDuration, easeInDuration, easeOutDuration, smoothFactor)
    endif
endfunction

//===========================================================================

/**
Swapped arguments of `CameraSetupGetField` for WE usage.

@patch 1.00
*/
function CameraSetupGetFieldSwap takes camerafield whichField, camerasetup whichSetup returns real
    return CameraSetupGetField(whichSetup, whichField)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraFieldForPlayer takes player whichPlayer, camerafield whichField, real value, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraField(whichField, value, duration)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraTargetControllerNoZForPlayer takes player whichPlayer, unit whichUnit, real xoffset, real yoffset, boolean inheritOrientation returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraTargetController(whichUnit, xoffset, yoffset, inheritOrientation)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraPositionForPlayer takes player whichPlayer, real x, real y returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraPosition(x, y)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraPositionLocForPlayer takes player whichPlayer, location loc returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraPosition(GetLocationX(loc), GetLocationY(loc))
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RotateCameraAroundLocBJ takes real degrees, location loc, player whichPlayer, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraRotateMode(GetLocationX(loc), GetLocationY(loc), bj_DEGTORAD * degrees, duration)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PanCameraToForPlayer takes player whichPlayer, real x, real y returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call PanCameraTo(x, y)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PanCameraToLocForPlayer takes player whichPlayer, location loc returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call PanCameraTo(GetLocationX(loc), GetLocationY(loc))
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PanCameraToTimedForPlayer takes player whichPlayer, real x, real y, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call PanCameraToTimed(x, y, duration)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PanCameraToTimedLocForPlayer takes player whichPlayer, location loc, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call PanCameraToTimed(GetLocationX(loc), GetLocationY(loc), duration)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PanCameraToTimedLocWithZForPlayer takes player whichPlayer, location loc, real zOffset, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call PanCameraToTimedWithZ(GetLocationX(loc), GetLocationY(loc), zOffset, duration)
    endif
endfunction

//===========================================================================

/**
@bug Fixed in 1.31: Caused a desync in multiplayer by creating a `location` inside
the local block. It was known as "Camera - Pan Camera as necessary (timed)" in GUI.
[Explanation](https://www.hiveworkshop.com/threads/fixing-smartcamerapanbj-desync.243334/)

@bug Leaks handle `cameraLoc`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function SmartCameraPanBJ takes player whichPlayer, location loc, real duration returns nothing
    local real dist
	local location cameraLoc = GetCameraTargetPositionLoc()
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.

        set dist = DistanceBetweenPoints(loc, cameraLoc)
        if (dist >= bj_SMARTPAN_TRESHOLD_SNAP) then
            // If the user is too far away, snap the camera.
            call PanCameraToTimed(GetLocationX(loc), GetLocationY(loc), 0)
        elseif (dist >= bj_SMARTPAN_TRESHOLD_PAN) then
            // If the user is moderately close, pan the camera.
            call PanCameraToTimed(GetLocationX(loc), GetLocationY(loc), duration)
        else
            // User is close enough, so don't touch the camera.
        endif
    endif
	call RemoveLocation(cameraLoc)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCinematicCameraForPlayer takes player whichPlayer, string cameraModelFile returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCinematicCamera(cameraModelFile)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ResetToGameCameraForPlayer takes player whichPlayer, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ResetToGameCamera(duration)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CameraSetSourceNoiseForPlayer takes player whichPlayer, real magnitude, real velocity returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call CameraSetSourceNoise(magnitude, velocity)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CameraSetTargetNoiseForPlayer takes player whichPlayer, real magnitude, real velocity returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call CameraSetTargetNoise(magnitude, velocity)
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function CameraSetEQNoiseForPlayer takes player whichPlayer, real magnitude returns nothing
    local real richter = magnitude
    if (richter > 5.0) then
        set richter = 5.0
    endif
    if (richter < 2.0) then
        set richter = 2.0
    endif
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call CameraSetTargetNoiseEx(magnitude*2.0, magnitude*Pow(10,richter),true)
        call CameraSetSourceNoiseEx(magnitude*2.0, magnitude*Pow(10,richter),true)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CameraClearNoiseForPlayer takes player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call CameraSetSourceNoise(0, 0)
        call CameraSetTargetNoise(0, 0)
    endif
endfunction

//===========================================================================
// Query the current camera bounds.
//

/**
@patch 1.00
*/
function GetCurrentCameraBoundsMapRectBJ takes nothing returns rect
    return Rect(GetCameraBoundMinX(), GetCameraBoundMinY(), GetCameraBoundMaxX(), GetCameraBoundMaxY())
endfunction

//===========================================================================
// Query the initial camera bounds, as defined at map init.
//

/**
@patch 1.00
*/
function GetCameraBoundsMapRect takes nothing returns rect
    return bj_mapInitialCameraBounds
endfunction

//===========================================================================
// Query the playable map area, as defined at map init.
//

/**
@patch 1.00
*/
function GetPlayableMapRect takes nothing returns rect
    return bj_mapInitialPlayableArea
endfunction

//===========================================================================
// Query the entire map area, as defined at map init.
//

/**
@patch 1.00
*/
function GetEntireMapRect takes nothing returns rect
    return GetWorldBounds()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraBoundsToRect takes rect r returns nothing
    local real minX = GetRectMinX(r)
    local real minY = GetRectMinY(r)
    local real maxX = GetRectMaxX(r)
    local real maxY = GetRectMaxY(r)
    call SetCameraBounds(minX, minY, minX, maxY, maxX, maxY, maxX, minY)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraBoundsToRectForPlayerBJ takes player whichPlayer, rect r returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraBoundsToRect(r)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AdjustCameraBoundsBJ takes integer adjustMethod, real dxWest, real dxEast, real dyNorth, real dySouth returns nothing
    local real minX = 0
    local real minY = 0
    local real maxX = 0
    local real maxY = 0
    local real scale = 0

    if (adjustMethod == bj_CAMERABOUNDS_ADJUST_ADD) then
        set scale = 1
    elseif (adjustMethod == bj_CAMERABOUNDS_ADJUST_SUB) then
        set scale = -1
    else
        // Unrecognized adjustment method - ignore the request.
        return
    endif

    // Adjust the actual camera values
    set minX = GetCameraBoundMinX() - scale * dxWest
    set maxX = GetCameraBoundMaxX() + scale * dxEast
    set minY = GetCameraBoundMinY() - scale * dySouth
    set maxY = GetCameraBoundMaxY() + scale * dyNorth

    // Make sure the camera bounds are still valid.
    if (maxX < minX) then
        set minX = (minX + maxX) * 0.5
        set maxX = minX
    endif
    if (maxY < minY) then
        set minY = (minY + maxY) * 0.5
        set maxY = minY
    endif

    // Apply the new camera values.
    call SetCameraBounds(minX, minY, minX, maxY, maxX, maxY, maxX, minY)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AdjustCameraBoundsForPlayerBJ takes integer adjustMethod, player whichPlayer, real dxWest, real dxEast, real dyNorth, real dySouth returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call AdjustCameraBoundsBJ(adjustMethod, dxWest, dxEast, dyNorth, dySouth)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraQuickPositionForPlayer takes player whichPlayer, real x, real y returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraQuickPosition(x, y)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraQuickPositionLocForPlayer takes player whichPlayer, location loc returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraQuickPosition(GetLocationX(loc), GetLocationY(loc))
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraQuickPositionLoc takes location loc returns nothing
    call SetCameraQuickPosition(GetLocationX(loc), GetLocationY(loc))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StopCameraForPlayerBJ takes player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call StopCamera()
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCameraOrientControllerForPlayerBJ takes player whichPlayer, unit whichUnit, real xoffset, real yoffset returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetCameraOrientController(whichUnit, xoffset, yoffset)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CameraSetSmoothingFactorBJ takes real factor returns nothing
    call CameraSetSmoothingFactor(factor)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CameraResetSmoothingFactorBJ takes nothing returns nothing
    call CameraSetSmoothingFactor(0)
endfunction



//***************************************************************************
//*
//*  Text Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function DisplayTextToForce takes force toForce, string message returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), toForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call DisplayTextToPlayer(GetLocalPlayer(), 0, 0, message)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DisplayTimedTextToForce takes force toForce, real duration, string message returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), toForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, duration, message)
    endif
endfunction

//===========================================================================

/**
Clears text messages for all players in specified force.

@param toForce Target players who are part of this force

@note See `ClearTextMessages`.

@patch 1.00
*/
function ClearTextMessagesBJ takes force toForce returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), toForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ClearTextMessages()
    endif
endfunction

//===========================================================================
// The parameters for the API Substring function are unintuitive, so this
// merely performs a translation for the starting index.
//

/**
@patch 1.00
*/
function SubStringBJ takes string source, integer start, integer end returns string
    return SubString(source, start-1, end)
endfunction  
  

/**
Equivalent to `GetHandleId`.

@patch 1.24a
*/
function GetHandleIdBJ takes handle h returns integer
    return GetHandleId(h)
endfunction


/**
Equivalent to `StringHash`.

@patch 1.24a
*/
function StringHashBJ takes string s returns integer
    return StringHash(s)
endfunction



//***************************************************************************
//*
//*  Event Registration Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterTimerEventPeriodic takes trigger trig, real timeout returns event
    return TriggerRegisterTimerEvent(trig, timeout, true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterTimerEventSingle takes trigger trig, real timeout returns event
    return TriggerRegisterTimerEvent(trig, timeout, false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterTimerExpireEventBJ takes trigger trig, timer t returns event
    return TriggerRegisterTimerExpireEvent(trig, t)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterPlayerUnitEventSimple takes trigger trig, player whichPlayer, playerunitevent whichEvent returns event
    return TriggerRegisterPlayerUnitEvent(trig, whichPlayer, whichEvent, null)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TriggerRegisterAnyUnitEventBJ takes trigger trig, playerunitevent whichEvent returns nothing
    local integer index

    set index = 0
    loop
        call TriggerRegisterPlayerUnitEvent(trig, Player(index), whichEvent, null)

        set index = index + 1
        exitwhen index == bj_MAX_PLAYER_SLOTS
    endloop
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterPlayerSelectionEventBJ takes trigger trig, player whichPlayer, boolean selected returns event
    if selected then
        return TriggerRegisterPlayerUnitEvent(trig, whichPlayer, EVENT_PLAYER_UNIT_SELECTED, null)
    else
        return TriggerRegisterPlayerUnitEvent(trig, whichPlayer, EVENT_PLAYER_UNIT_DESELECTED, null)
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TriggerRegisterPlayerKeyEventBJ takes trigger trig, player whichPlayer, integer keType, integer keKey returns event
    if (keType == bj_KEYEVENTTYPE_DEPRESS) then
        // Depress event - find out what key
        if (keKey == bj_KEYEVENTKEY_LEFT) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_LEFT_DOWN)
        elseif (keKey == bj_KEYEVENTKEY_RIGHT) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_RIGHT_DOWN)
        elseif (keKey == bj_KEYEVENTKEY_DOWN) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_DOWN_DOWN)
        elseif (keKey == bj_KEYEVENTKEY_UP) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_UP_DOWN)
        else
            // Unrecognized key - ignore the request and return failure.
            return null
        endif
    elseif (keType == bj_KEYEVENTTYPE_RELEASE) then
        // Release event - find out what key
        if (keKey == bj_KEYEVENTKEY_LEFT) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_LEFT_UP)
        elseif (keKey == bj_KEYEVENTKEY_RIGHT) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_RIGHT_UP)
        elseif (keKey == bj_KEYEVENTKEY_DOWN) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_DOWN_UP)
        elseif (keKey == bj_KEYEVENTKEY_UP) then
            return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ARROW_UP_UP)
        else
            // Unrecognized key - ignore the request and return failure.
            return null
        endif
    else
        // Unrecognized type - ignore the request and return failure.
        return null
    endif
endfunction

//===========================================================================

/**
@patch 1.29.0.8803
*/
function TriggerRegisterPlayerMouseEventBJ takes trigger trig, player whichPlayer, integer meType returns event
     if (meType == bj_MOUSEEVENTTYPE_DOWN) then
        // Mouse down event
        return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_MOUSE_DOWN)
    elseif (meType == bj_MOUSEEVENTTYPE_UP) then
        // Mouse up event
        return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_MOUSE_UP)
    elseif (meType == bj_MOUSEEVENTTYPE_MOVE) then
        // Mouse move event
        return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_MOUSE_MOVE)
    else
        // Unrecognized type - ignore the request and return failure.
         return null
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterPlayerEventVictory takes trigger trig, player whichPlayer returns event
    return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_VICTORY)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterPlayerEventDefeat takes trigger trig, player whichPlayer returns event
    return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_DEFEAT)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function TriggerRegisterPlayerEventLeave takes trigger trig, player whichPlayer returns event
    return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_LEAVE)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterPlayerEventAllianceChanged takes trigger trig, player whichPlayer returns event
    return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_ALLIANCE_CHANGED)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterPlayerEventEndCinematic takes trigger trig, player whichPlayer returns event
    return TriggerRegisterPlayerEvent(trig, whichPlayer, EVENT_PLAYER_END_CINEMATIC)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterGameStateEventTimeOfDay takes trigger trig, limitop opcode, real limitval returns event
    return TriggerRegisterGameStateEvent(trig, GAME_STATE_TIME_OF_DAY, opcode, limitval)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterEnterRegionSimple takes trigger trig, region whichRegion returns event
    return TriggerRegisterEnterRegion(trig, whichRegion, null)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterLeaveRegionSimple takes trigger trig, region whichRegion returns event
    return TriggerRegisterLeaveRegion(trig, whichRegion, null)
endfunction

//===========================================================================

/**
@bug Leaks handle `rectRegion`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function TriggerRegisterEnterRectSimple takes trigger trig, rect r returns event
    local region rectRegion = CreateRegion()
    call RegionAddRect(rectRegion, r)
    return TriggerRegisterEnterRegion(trig, rectRegion, null)
endfunction

//===========================================================================

/**
@bug Leaks handle `rectRegion`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function TriggerRegisterLeaveRectSimple takes trigger trig, rect r returns event
    local region rectRegion = CreateRegion()
    call RegionAddRect(rectRegion, r)
    return TriggerRegisterLeaveRegion(trig, rectRegion, null)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterDistanceBetweenUnits takes trigger trig, unit whichUnit, boolexpr condition, real range returns event
    return TriggerRegisterUnitInRange(trig, whichUnit, range, condition)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterUnitInRangeSimple takes trigger trig, real range, unit whichUnit returns event
    return TriggerRegisterUnitInRange(trig, whichUnit, range, null)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterUnitLifeEvent takes trigger trig, unit whichUnit, limitop opcode, real limitval returns event
    return TriggerRegisterUnitStateEvent(trig, whichUnit, UNIT_STATE_LIFE, opcode, limitval)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterUnitManaEvent takes trigger trig, unit whichUnit, limitop opcode, real limitval returns event
    return TriggerRegisterUnitStateEvent(trig, whichUnit, UNIT_STATE_MANA, opcode, limitval)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterDialogEventBJ takes trigger trig, dialog whichDialog returns event
    return TriggerRegisterDialogEvent(trig, whichDialog)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterShowSkillEventBJ takes trigger trig returns event
    return TriggerRegisterGameEvent(trig, EVENT_GAME_SHOW_SKILL)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TriggerRegisterBuildSubmenuEventBJ takes trigger trig returns event
    return TriggerRegisterGameEvent(trig, EVENT_GAME_BUILD_SUBMENU)
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function TriggerRegisterBuildCommandEventBJ takes trigger trig, integer unitId returns event
	call TriggerRegisterCommandEvent(trig, 'ANbu', UnitId2String(unitId))
	call TriggerRegisterCommandEvent(trig, 'AHbu', UnitId2String(unitId))
	call TriggerRegisterCommandEvent(trig, 'AEbu', UnitId2String(unitId))
	call TriggerRegisterCommandEvent(trig, 'AObu', UnitId2String(unitId))
	call TriggerRegisterCommandEvent(trig, 'AUbu', UnitId2String(unitId))
    return TriggerRegisterCommandEvent(trig, 'AGbu', UnitId2String(unitId))
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function TriggerRegisterTrainCommandEventBJ takes trigger trig, integer unitId returns event
    return TriggerRegisterCommandEvent(trig, 'Aque', UnitId2String(unitId))
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function TriggerRegisterUpgradeCommandEventBJ takes trigger trig, integer techId returns event
    return TriggerRegisterUpgradeCommandEvent(trig, techId)
endfunction

//===========================================================================

/**
@patch 1.32.0.14411
*/
function TriggerRegisterCommonCommandEventBJ takes trigger trig, string order returns event
    return TriggerRegisterCommandEvent(trig, 0, order)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TriggerRegisterGameLoadedEventBJ takes trigger trig returns event
    return TriggerRegisterGameEvent(trig, EVENT_GAME_LOADED)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TriggerRegisterGameSavedEventBJ takes trigger trig returns event
    return TriggerRegisterGameEvent(trig, EVENT_GAME_SAVE)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RegisterDestDeathInRegionEnum takes nothing returns nothing
    set bj_destInRegionDiesCount = bj_destInRegionDiesCount + 1
    if (bj_destInRegionDiesCount <= bj_MAX_DEST_IN_REGION_EVENTS) then
        call TriggerRegisterDeathEvent(bj_destInRegionDiesTrig, GetEnumDestructable())
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TriggerRegisterDestDeathInRegionEvent takes trigger trig, rect r returns nothing
    set bj_destInRegionDiesTrig = trig
    set bj_destInRegionDiesCount = 0
    call EnumDestructablesInRect(r, null, function RegisterDestDeathInRegionEnum)
endfunction



//***************************************************************************
//*
//*  Environment Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function AddWeatherEffectSaveLast takes rect where, integer effectID returns weathereffect
    set bj_lastCreatedWeatherEffect = AddWeatherEffect(where, effectID)
    return bj_lastCreatedWeatherEffect
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedWeatherEffect takes nothing returns weathereffect
    return bj_lastCreatedWeatherEffect
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RemoveWeatherEffectBJ takes weathereffect whichWeatherEffect returns nothing
    call RemoveWeatherEffect(whichWeatherEffect)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TerrainDeformationCraterBJ takes real duration, boolean permanent, location where, real radius, real depth returns terraindeformation
    set bj_lastCreatedTerrainDeformation = TerrainDeformCrater(GetLocationX(where), GetLocationY(where), radius, depth, R2I(duration * 1000), permanent)
    return bj_lastCreatedTerrainDeformation
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TerrainDeformationRippleBJ takes real duration, boolean limitNeg, location where, real startRadius, real endRadius, real depth, real wavePeriod, real waveWidth returns terraindeformation
    local real spaceWave
    local real timeWave
    local real radiusRatio

    if (endRadius <= 0 or waveWidth <= 0 or wavePeriod <= 0) then
        return null
    endif

    set timeWave = 2.0 * duration / wavePeriod
    set spaceWave = 2.0 * endRadius / waveWidth
    set radiusRatio = startRadius / endRadius

    set bj_lastCreatedTerrainDeformation = TerrainDeformRipple(GetLocationX(where), GetLocationY(where), endRadius, depth, R2I(duration * 1000), 1, spaceWave, timeWave, radiusRatio, limitNeg)
    return bj_lastCreatedTerrainDeformation
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TerrainDeformationWaveBJ takes real duration, location source, location target, real radius, real depth, real trailDelay returns terraindeformation
    local real distance
    local real dirX
    local real dirY
    local real speed

    set distance = DistanceBetweenPoints(source, target)
    if (distance == 0 or duration <= 0) then
        return null
    endif

    set dirX = (GetLocationX(target) - GetLocationX(source)) / distance
    set dirY = (GetLocationY(target) - GetLocationY(source)) / distance
    set speed = distance / duration

    set bj_lastCreatedTerrainDeformation = TerrainDeformWave(GetLocationX(source), GetLocationY(source), dirX, dirY, distance, speed, radius, depth, R2I(trailDelay * 1000), 1)
    return bj_lastCreatedTerrainDeformation
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TerrainDeformationRandomBJ takes real duration, location where, real radius, real minDelta, real maxDelta, real updateInterval returns terraindeformation
    set bj_lastCreatedTerrainDeformation = TerrainDeformRandom(GetLocationX(where), GetLocationY(where), radius, minDelta, maxDelta, R2I(duration * 1000), R2I(updateInterval * 1000))
    return bj_lastCreatedTerrainDeformation
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TerrainDeformationStopBJ takes terraindeformation deformation, real duration returns nothing
    call TerrainDeformStop(deformation, R2I(duration * 1000))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function GetLastCreatedTerrainDeformation takes nothing returns terraindeformation
    return bj_lastCreatedTerrainDeformation
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function AddLightningLoc takes string codeName, location where1, location where2 returns lightning
    set bj_lastCreatedLightning = AddLightningEx(codeName, true, GetLocationX(where1), GetLocationY(where1), GetLocationZ(where1), GetLocationX(where2), GetLocationY(where2), GetLocationZ(where2))
    return bj_lastCreatedLightning
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function DestroyLightningBJ takes lightning whichBolt returns boolean
    return DestroyLightning(whichBolt)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function MoveLightningLoc takes lightning whichBolt, location where1, location where2 returns boolean
    return MoveLightningEx(whichBolt, true, GetLocationX(where1), GetLocationY(where1), GetLocationZ(where1), GetLocationX(where2), GetLocationY(where2), GetLocationZ(where2))
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function GetLightningColorABJ takes lightning whichBolt returns real
    return GetLightningColorA(whichBolt)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function GetLightningColorRBJ takes lightning whichBolt returns real
    return GetLightningColorR(whichBolt)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function GetLightningColorGBJ takes lightning whichBolt returns real
    return GetLightningColorG(whichBolt)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function GetLightningColorBBJ takes lightning whichBolt returns real
    return GetLightningColorB(whichBolt)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function SetLightningColorBJ takes lightning whichBolt, real r, real g, real b, real a returns boolean
    return SetLightningColor(whichBolt, r, g, b, a)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function GetLastCreatedLightningBJ takes nothing returns lightning
    return bj_lastCreatedLightning
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function GetAbilityEffectBJ takes integer abilcode, effecttype t, integer index returns string
    return GetAbilityEffectById(abilcode, t, index)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function GetAbilitySoundBJ takes integer abilcode, soundtype t returns string
    return GetAbilitySoundById(abilcode, t)
endfunction


//===========================================================================

/**
@patch 1.07
*/
function GetTerrainCliffLevelBJ takes location where returns integer
    return GetTerrainCliffLevel(GetLocationX(where), GetLocationY(where))
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function GetTerrainTypeBJ takes location where returns integer
    return GetTerrainType(GetLocationX(where), GetLocationY(where))
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function GetTerrainVarianceBJ takes location where returns integer
    return GetTerrainVariance(GetLocationX(where), GetLocationY(where))
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTerrainTypeBJ takes location where, integer terrainType, integer variation, integer area, integer shape returns nothing
    call SetTerrainType(GetLocationX(where), GetLocationY(where), terrainType, variation, area, shape)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function IsTerrainPathableBJ takes location where, pathingtype t returns boolean
    return IsTerrainPathable(GetLocationX(where), GetLocationY(where), t)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTerrainPathableBJ takes location where, pathingtype t, boolean flag returns nothing
    call SetTerrainPathable(GetLocationX(where), GetLocationY(where), t, flag)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetWaterBaseColorBJ takes real red, real green, real blue, real transparency returns nothing
    call SetWaterBaseColor(PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateFogModifierRectSimple takes player whichPlayer, fogstate whichFogState, rect r, boolean afterUnits returns fogmodifier
    set bj_lastCreatedFogModifier = CreateFogModifierRect(whichPlayer, whichFogState, r, true, afterUnits)
    return bj_lastCreatedFogModifier
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateFogModifierRadiusLocSimple takes player whichPlayer, fogstate whichFogState, location center, real radius, boolean afterUnits returns fogmodifier
    set bj_lastCreatedFogModifier = CreateFogModifierRadiusLoc(whichPlayer, whichFogState, center, radius, true, afterUnits)
    return bj_lastCreatedFogModifier
endfunction

//===========================================================================
// Version of CreateFogModifierRect that assumes use of sharedVision and
// gives the option of immediately enabling the modifier, so that triggers
// can default to modifiers that are immediately enabled.
//

/**
@patch 1.00
*/
function CreateFogModifierRectBJ takes boolean enabled, player whichPlayer, fogstate whichFogState, rect r returns fogmodifier
    set bj_lastCreatedFogModifier = CreateFogModifierRect(whichPlayer, whichFogState, r, true, false)
    if enabled then
        call FogModifierStart(bj_lastCreatedFogModifier)
    endif
    return bj_lastCreatedFogModifier
endfunction

//===========================================================================
// Version of CreateFogModifierRadius that assumes use of sharedVision and
// gives the option of immediately enabling the modifier, so that triggers
// can default to modifiers that are immediately enabled.
//

/**
@patch 1.00
*/
function CreateFogModifierRadiusLocBJ takes boolean enabled, player whichPlayer, fogstate whichFogState, location center, real radius returns fogmodifier
    set bj_lastCreatedFogModifier = CreateFogModifierRadiusLoc(whichPlayer, whichFogState, center, radius, true, false)
    if enabled then
        call FogModifierStart(bj_lastCreatedFogModifier)
    endif
    return bj_lastCreatedFogModifier
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedFogModifier takes nothing returns fogmodifier
    return bj_lastCreatedFogModifier
endfunction

//===========================================================================

/**
@patch 1.00
*/
function FogEnableOn takes nothing returns nothing
    call FogEnable(true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function FogEnableOff takes nothing returns nothing
    call FogEnable(false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function FogMaskEnableOn takes nothing returns nothing
    call FogMaskEnable(true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function FogMaskEnableOff takes nothing returns nothing
    call FogMaskEnable(false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UseTimeOfDayBJ takes boolean flag returns nothing
    call SuspendTimeOfDay(not flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetTerrainFogExBJ takes integer style, real zstart, real zend, real density, real red, real green, real blue returns nothing
    call SetTerrainFogEx(style, zstart, zend, density, red * 0.01, green * 0.01, blue * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ResetTerrainFogBJ takes nothing returns nothing
    call ResetTerrainFog()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDoodadAnimationBJ takes string animName, integer doodadID, real radius, location center returns nothing
    call SetDoodadAnimation(GetLocationX(center), GetLocationY(center), radius, doodadID, false, animName, false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDoodadAnimationRectBJ takes string animName, integer doodadID, rect r returns nothing
    call SetDoodadAnimationRect(r, doodadID, animName, false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AddUnitAnimationPropertiesBJ takes boolean add, string animProperties, unit whichUnit returns nothing
    call AddUnitAnimationProperties(whichUnit, animProperties, add)
endfunction


//============================================================================

/**
@patch 1.18a
*/
function CreateImageBJ takes string file, real size, location where, real zOffset, integer imageType returns image
    set bj_lastCreatedImage = CreateImage(file, size, size, size, GetLocationX(where), GetLocationY(where), zOffset, 0, 0, 0, imageType)
    return bj_lastCreatedImage
endfunction

//============================================================================

/**
@patch 1.18a
*/
function ShowImageBJ takes boolean flag, image whichImage returns nothing
    call ShowImage(whichImage, flag)
endfunction

//============================================================================

/**
See: `SetImagePosition`. The only difference: takes a location instead of X, Y coordinates.

@patch 1.18a
*/
function SetImagePositionBJ takes image whichImage, location where, real zOffset returns nothing
    call SetImagePosition(whichImage, GetLocationX(where), GetLocationY(where), zOffset)
endfunction

//============================================================================

/**
Similar to SetImageColor, however this takes 0-100 values as percent for colors. Alpha channel=(100-alpha)

@patch 1.18a
*/
function SetImageColorBJ takes image whichImage, real red, real green, real blue, real alpha returns nothing
    call SetImageColor(whichImage, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-alpha))
endfunction

//============================================================================

/**
@patch 1.18a
*/
function GetLastCreatedImage takes nothing returns image
    return bj_lastCreatedImage
endfunction

//============================================================================

/**
@patch 1.18a
*/
function CreateUbersplatBJ takes location where, string name, real red, real green, real blue, real alpha, boolean forcePaused, boolean noBirthTime returns ubersplat
    set bj_lastCreatedUbersplat = CreateUbersplat(GetLocationX(where), GetLocationY(where), name, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-alpha), forcePaused, noBirthTime)
    return bj_lastCreatedUbersplat
endfunction

//============================================================================

/**
@patch 1.18a
*/
function ShowUbersplatBJ takes boolean flag, ubersplat whichSplat returns nothing
    call ShowUbersplat(whichSplat, flag)
endfunction

//============================================================================

/**
@patch 1.18a
*/
function GetLastCreatedUbersplat takes nothing returns ubersplat
    return bj_lastCreatedUbersplat
endfunction

//============================================================================

/**
@patch 1.32.0.13369
*/
function GetLastCreatedMinimapIcon takes nothing returns minimapicon
    return bj_lastCreatedMinimapIcon
endfunction

//============================================================================

/**
@patch 1.32.0.13369
*/
function CreateMinimapIconOnUnitBJ takes unit whichUnit, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon
    set bj_lastCreatedMinimapIcon = CreateMinimapIconOnUnit(whichUnit, red, green, blue, pingPath, fogVisibility)
    return bj_lastCreatedMinimapIcon
endfunction

//============================================================================

/**
@patch 1.32.0.13369
*/
function CreateMinimapIconAtLocBJ takes location where, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon
    set bj_lastCreatedMinimapIcon = CreateMinimapIconAtLoc(where, red, green, blue, pingPath, fogVisibility)
    return bj_lastCreatedMinimapIcon
endfunction

//============================================================================

/**
@patch 1.32.0.13369
*/
function CreateMinimapIconBJ takes real x, real y, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon
    set bj_lastCreatedMinimapIcon = CreateMinimapIcon(x, y, red, green, blue, pingPath, fogVisibility)
    return bj_lastCreatedMinimapIcon
endfunction

//============================================================================

/**
@patch 1.32.0.13369
*/
function CampaignMinimapIconUnitBJ takes unit whichUnit, integer style returns nothing
	local integer	red
	local integer 	green
	local integer 	blue
	local string 	path
	if ( style == bj_CAMPPINGSTYLE_PRIMARY ) then
		// green
		set red 	= 255
		set green 	= 0
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectivePrimary" )
	elseif ( style == bj_CAMPPINGSTYLE_PRIMARY_GREEN ) then
		// green
		set red 	= 0
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectivePrimary" )
	elseif ( style == bj_CAMPPINGSTYLE_PRIMARY_RED ) then
		// green
		set red 	= 255
		set green 	= 0
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectivePrimary" )
	elseif ( style == bj_CAMPPINGSTYLE_BONUS ) then
		// yellow
		set red 	= 255
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectiveBonus" )
	elseif ( style == bj_CAMPPINGSTYLE_TURNIN ) then
		// yellow
		set red 	= 255
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestTurnIn" )
	elseif ( style == bj_CAMPPINGSTYLE_BOSS ) then
		// red
		set red 	= 255
		set green 	= 0
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestBoss" )
	elseif ( style == bj_CAMPPINGSTYLE_CONTROL_ALLY ) then
		// green
		set red 	= 0
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestControlPoint" )
	elseif ( style == bj_CAMPPINGSTYLE_CONTROL_NEUTRAL ) then
		// white
		set red 	= 255
		set green 	= 255
		set blue	= 255
		set path	= SkinManagerGetLocalPath( "MinimapQuestControlPoint" )
	elseif ( style == bj_CAMPPINGSTYLE_CONTROL_ENEMY ) then
		// red
		set red 	= 255
		set green 	= 0
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestControlPoint" )
	endif
	call CreateMinimapIconOnUnitBJ( whichUnit, red, green, blue, path, FOG_OF_WAR_MASKED )
    call SetMinimapIconOrphanDestroy( bj_lastCreatedMinimapIcon, true )
endfunction


//============================================================================

/**
@patch 1.32.0.13369
*/
function CampaignMinimapIconLocBJ takes location where, integer style returns nothing
	local integer	red
	local integer 	green
	local integer 	blue
	local string 	path
	if ( style == bj_CAMPPINGSTYLE_PRIMARY ) then
		// green (different from the unit version)
		set red 	= 0
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectivePrimary" )
	elseif ( style == bj_CAMPPINGSTYLE_PRIMARY_GREEN ) then
		// green (different from the unit version)
		set red 	= 0
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectivePrimary" )
	elseif ( style == bj_CAMPPINGSTYLE_PRIMARY_RED ) then
		// green (different from the unit version)
		set red 	= 255
		set green 	= 0
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectivePrimary" )
	elseif ( style == bj_CAMPPINGSTYLE_BONUS ) then
		// yellow
		set red 	= 255
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestObjectiveBonus" )
	elseif ( style == bj_CAMPPINGSTYLE_TURNIN ) then
		// yellow
		set red 	= 255
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestTurnIn" )
	elseif ( style == bj_CAMPPINGSTYLE_BOSS ) then
		// red
		set red 	= 255
		set green 	= 0
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestBoss" )
	elseif ( style == bj_CAMPPINGSTYLE_CONTROL_ALLY ) then
		// green
		set red 	= 0
		set green 	= 255
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestControlPoint" )
	elseif ( style == bj_CAMPPINGSTYLE_CONTROL_NEUTRAL ) then
		// white
		set red 	= 255
		set green 	= 255
		set blue	= 255
		set path	= SkinManagerGetLocalPath( "MinimapQuestControlPoint" )
	elseif ( style == bj_CAMPPINGSTYLE_CONTROL_ENEMY ) then
		// red
		set red 	= 255
		set green 	= 0
		set blue	= 0
		set path	= SkinManagerGetLocalPath( "MinimapQuestControlPoint" )
	endif
	call CreateMinimapIconAtLocBJ( where, red, green, blue, path, FOG_OF_WAR_MASKED )
endfunction


//***************************************************************************
//*
//*  Sound Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function PlaySoundBJ takes sound soundHandle returns nothing
    set bj_lastPlayedSound = soundHandle
    if (soundHandle != null) then
        call StartSound(soundHandle)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StopSoundBJ takes sound soundHandle, boolean fadeOut returns nothing
    call StopSound(soundHandle, false, fadeOut)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetSoundVolumeBJ takes sound soundHandle, real volumePercent returns nothing
    call SetSoundVolume(soundHandle, PercentToInt(volumePercent, 127))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetSoundOffsetBJ takes real newOffset, sound soundHandle returns nothing
    call SetSoundPlayPosition(soundHandle, R2I(newOffset * 1000))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetSoundDistanceCutoffBJ takes sound soundHandle, real cutoff returns nothing
    call SetSoundDistanceCutoff(soundHandle, cutoff)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetSoundPitchBJ takes sound soundHandle, real pitch returns nothing
    call SetSoundPitch(soundHandle, pitch)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetSoundPositionLocBJ takes sound soundHandle, location loc, real z returns nothing
    call SetSoundPosition(soundHandle, GetLocationX(loc), GetLocationY(loc), z)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AttachSoundToUnitBJ takes sound soundHandle, unit whichUnit returns nothing
    call AttachSoundToUnit(soundHandle, whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetSoundConeAnglesBJ takes sound soundHandle, real inside, real outside, real outsideVolumePercent returns nothing
    call SetSoundConeAngles(soundHandle, inside, outside, PercentToInt(outsideVolumePercent, 127))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function KillSoundWhenDoneBJ takes sound soundHandle returns nothing
    call KillSoundWhenDone(soundHandle)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function PlaySoundAtPointBJ takes sound soundHandle, real volumePercent, location loc, real z returns nothing
    call SetSoundPositionLocBJ(soundHandle, loc, z)
    call SetSoundVolumeBJ(soundHandle, volumePercent)
    call PlaySoundBJ(soundHandle)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function PlaySoundOnUnitBJ takes sound soundHandle, real volumePercent, unit whichUnit returns nothing
    call AttachSoundToUnitBJ(soundHandle, whichUnit)
    call SetSoundVolumeBJ(soundHandle, volumePercent)
    call PlaySoundBJ(soundHandle)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function PlaySoundFromOffsetBJ takes sound soundHandle, real volumePercent, real startingOffset returns nothing
    call SetSoundVolumeBJ(soundHandle, volumePercent)
    call PlaySoundBJ(soundHandle)
    call SetSoundOffsetBJ(startingOffset, soundHandle)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PlayMusicBJ takes string musicFileName returns nothing
    set bj_lastPlayedMusic = musicFileName
    call PlayMusic(musicFileName)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function PlayMusicExBJ takes string musicFileName, real startingOffset, real fadeInTime returns nothing
    set bj_lastPlayedMusic = musicFileName
    call PlayMusicEx(musicFileName, R2I(startingOffset * 1000), R2I(fadeInTime * 1000))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetMusicOffsetBJ takes real newOffset returns nothing
    call SetMusicPlayPosition(R2I(newOffset * 1000))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PlayThematicMusicBJ takes string musicName returns nothing
    call PlayThematicMusic(musicName)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function PlayThematicMusicExBJ takes string musicName, real startingOffset returns nothing
    call PlayThematicMusicEx(musicName, R2I(startingOffset * 1000))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetThematicMusicOffsetBJ takes real newOffset returns nothing
    call SetThematicMusicPlayPosition(R2I(newOffset * 1000))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function EndThematicMusicBJ takes nothing returns nothing
    call EndThematicMusic()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StopMusicBJ takes boolean fadeOut returns nothing
    call StopMusic(fadeOut)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ResumeMusicBJ takes nothing returns nothing
    call ResumeMusic()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetMusicVolumeBJ takes real volumePercent returns nothing
    call SetMusicVolume(PercentToInt(volumePercent, 127))
endfunction

//===========================================================================

/**
@patch 1.32.1.14604
*/
function SetThematicMusicVolumeBJ takes real volumePercent returns nothing
    call SetThematicMusicVolume(PercentToInt(volumePercent, 127))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetSoundDurationBJ takes sound soundHandle returns real
    if (soundHandle == null) then
        return bj_NOTHING_SOUND_DURATION
    else
        return I2R(GetSoundDuration(soundHandle)) * 0.001
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetSoundFileDurationBJ takes string musicFileName returns real
    return I2R(GetSoundFileDuration(musicFileName)) * 0.001
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastPlayedSound takes nothing returns sound
    return bj_lastPlayedSound
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastPlayedMusic takes nothing returns string
    return bj_lastPlayedMusic
endfunction

//===========================================================================

/**
@patch 1.00
*/
function VolumeGroupSetVolumeBJ takes volumegroup vgroup, real percent returns nothing
    call VolumeGroupSetVolume(vgroup, percent * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCineModeVolumeGroupsImmediateBJ takes nothing returns nothing
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_UNITMOVEMENT,  bj_CINEMODE_VOLUME_UNITMOVEMENT)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_UNITSOUNDS,    bj_CINEMODE_VOLUME_UNITSOUNDS)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_COMBAT,        bj_CINEMODE_VOLUME_COMBAT)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_SPELLS,        bj_CINEMODE_VOLUME_SPELLS)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_UI,            bj_CINEMODE_VOLUME_UI)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_MUSIC,         bj_CINEMODE_VOLUME_MUSIC)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_AMBIENTSOUNDS, bj_CINEMODE_VOLUME_AMBIENTSOUNDS)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_FIRE,          bj_CINEMODE_VOLUME_FIRE)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCineModeVolumeGroupsBJ takes nothing returns nothing
    // Delay the request if it occurs at map init.
    if bj_gameStarted then
        call SetCineModeVolumeGroupsImmediateBJ()
    else
        call TimerStart(bj_volumeGroupsTimer, bj_GAME_STARTED_THRESHOLD, false, function SetCineModeVolumeGroupsImmediateBJ)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetSpeechVolumeGroupsImmediateBJ takes nothing returns nothing
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_UNITMOVEMENT,  bj_SPEECH_VOLUME_UNITMOVEMENT)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_UNITSOUNDS,    bj_SPEECH_VOLUME_UNITSOUNDS)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_COMBAT,        bj_SPEECH_VOLUME_COMBAT)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_SPELLS,        bj_SPEECH_VOLUME_SPELLS)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_UI,            bj_SPEECH_VOLUME_UI)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_MUSIC,         bj_SPEECH_VOLUME_MUSIC)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_AMBIENTSOUNDS, bj_SPEECH_VOLUME_AMBIENTSOUNDS)
    call VolumeGroupSetVolume(SOUND_VOLUMEGROUP_FIRE,          bj_SPEECH_VOLUME_FIRE)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetSpeechVolumeGroupsBJ takes nothing returns nothing
    // Delay the request if it occurs at map init.
    if bj_gameStarted then
        call SetSpeechVolumeGroupsImmediateBJ()
    else
        call TimerStart(bj_volumeGroupsTimer, bj_GAME_STARTED_THRESHOLD, false, function SetSpeechVolumeGroupsImmediateBJ)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function VolumeGroupResetImmediateBJ takes nothing returns nothing
    call VolumeGroupReset()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function VolumeGroupResetBJ takes nothing returns nothing
    // Delay the request if it occurs at map init.
    if bj_gameStarted then
        call VolumeGroupResetImmediateBJ()
    else
        call TimerStart(bj_volumeGroupsTimer, bj_GAME_STARTED_THRESHOLD, false, function VolumeGroupResetImmediateBJ)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetSoundIsPlayingBJ takes sound soundHandle returns boolean
    return GetSoundIsLoading(soundHandle) or GetSoundIsPlaying(soundHandle)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function WaitForSoundBJ takes sound soundHandle, real offset returns nothing
    call TriggerWaitForSound( soundHandle, offset )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetMapMusicIndexedBJ takes string musicName, integer index returns nothing
    call SetMapMusic(musicName, false, index)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetMapMusicRandomBJ takes string musicName returns nothing
    call SetMapMusic(musicName, true, 0)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ClearMapMusicBJ takes nothing returns nothing
    call ClearMapMusic()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetStackedSoundBJ takes boolean add, sound soundHandle, rect r returns nothing
    local real width = GetRectMaxX(r) - GetRectMinX(r)
    local real height = GetRectMaxY(r) - GetRectMinY(r)

    call SetSoundPosition(soundHandle, GetRectCenterX(r), GetRectCenterY(r), 0)
    if add then
        call RegisterStackedSound(soundHandle, true, width, height)
    else
        call UnregisterStackedSound(soundHandle, true, width, height)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StartSoundForPlayerBJ takes player whichPlayer, sound soundHandle returns nothing
    if (whichPlayer == GetLocalPlayer()) then
        call StartSound(soundHandle)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function VolumeGroupSetVolumeForPlayerBJ takes player whichPlayer, volumegroup vgroup, real scale returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        call VolumeGroupSetVolume(vgroup, scale)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function EnableDawnDusk takes boolean flag returns nothing
    set bj_useDawnDuskSounds = flag
endfunction

//===========================================================================

/**
@patch 1.07
*/
function IsDawnDuskEnabled takes nothing returns boolean
    return bj_useDawnDuskSounds
endfunction



//***************************************************************************
//*
//*  Day/Night ambient sounds
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function SetAmbientDaySound takes string inLabel returns nothing
    local real ToD

    // Stop old sound, if necessary
    if (bj_dayAmbientSound != null) then
        call StopSound(bj_dayAmbientSound, true, true)
    endif

    // Create new sound
    set bj_dayAmbientSound = CreateMIDISound(inLabel, 20, 20)

    // Start the sound if necessary, based on current time
    set ToD = GetTimeOfDay()
    if (ToD >= bj_TOD_DAWN and ToD < bj_TOD_DUSK) then
        call StartSound(bj_dayAmbientSound)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetAmbientNightSound takes string inLabel returns nothing
    local real ToD

    // Stop old sound, if necessary
    if (bj_nightAmbientSound != null) then
        call StopSound(bj_nightAmbientSound, true, true)
    endif

    // Create new sound
    set bj_nightAmbientSound = CreateMIDISound(inLabel, 20, 20)

    // Start the sound if necessary, based on current time
    set ToD = GetTimeOfDay()
    if (ToD < bj_TOD_DAWN or ToD >= bj_TOD_DUSK) then
        call StartSound(bj_nightAmbientSound)
    endif
endfunction



//***************************************************************************
//*
//*  Special Effect Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function AddSpecialEffectLocBJ takes location where, string modelName returns effect
    set bj_lastCreatedEffect = AddSpecialEffectLoc(modelName, where)
    return bj_lastCreatedEffect
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AddSpecialEffectTargetUnitBJ takes string attachPointName, widget targetWidget, string modelName returns effect
    set bj_lastCreatedEffect = AddSpecialEffectTarget(modelName, targetWidget, attachPointName)
    return bj_lastCreatedEffect
endfunction

//===========================================================================
// Two distinct trigger actions can't share the same function name, so this
// dummy function simply mimics the behavior of an existing call.
//
// Commented out - Destructibles have no attachment points.
//
//function AddSpecialEffectTargetDestructableBJ takes string attachPointName, widget targetWidget, string modelName returns effect
//    return AddSpecialEffectTargetUnitBJ(attachPointName, targetWidget, modelName)
//endfunction

//===========================================================================
// Two distinct trigger actions can't share the same function name, so this
// dummy function simply mimics the behavior of an existing call.
//
// Commented out - Items have no attachment points.
//
//function AddSpecialEffectTargetItemBJ takes string attachPointName, widget targetWidget, string modelName returns effect
//    return AddSpecialEffectTargetUnitBJ(attachPointName, targetWidget, modelName)
//endfunction

//===========================================================================

/**
@patch 1.00
*/
function DestroyEffectBJ takes effect whichEffect returns nothing
    call DestroyEffect(whichEffect)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedEffectBJ takes nothing returns effect
    return bj_lastCreatedEffect
endfunction



//***************************************************************************
//*
//*  Command Button Effect Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.32.0.13369
*/
function CreateCommandButtonEffectBJ takes integer abilityId, string order returns commandbuttoneffect
    set bj_lastCreatedCommandButtonEffect = CreateCommandButtonEffect(abilityId, order)
    return bj_lastCreatedCommandButtonEffect
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function CreateTrainCommandButtonEffectBJ takes integer unitId returns commandbuttoneffect
    set bj_lastCreatedCommandButtonEffect = CreateCommandButtonEffect('Aque', UnitId2String(unitId))
    return bj_lastCreatedCommandButtonEffect
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function CreateUpgradeCommandButtonEffectBJ takes integer techId returns commandbuttoneffect
    set bj_lastCreatedCommandButtonEffect = CreateUpgradeCommandButtonEffect(techId)
    return bj_lastCreatedCommandButtonEffect
endfunction

//===========================================================================

/**
@patch 1.32.0.14411
*/
function CreateCommonCommandButtonEffectBJ takes string order returns commandbuttoneffect
    set bj_lastCreatedCommandButtonEffect = CreateCommandButtonEffect(0, order)
    return bj_lastCreatedCommandButtonEffect
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function CreateLearnCommandButtonEffectBJ takes integer abilityId returns commandbuttoneffect
    set bj_lastCreatedCommandButtonEffect = CreateLearnCommandButtonEffect(abilityId)
    return bj_lastCreatedCommandButtonEffect
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function CreateBuildCommandButtonEffectBJ takes integer unitId returns commandbuttoneffect
	local race r = GetPlayerRace(GetLocalPlayer())
	local integer abilityId
	if (r == RACE_HUMAN) then
        set abilityId = 'AHbu'
    elseif (r == RACE_ORC) then
        set abilityId = 'AObu'
    elseif (r == RACE_UNDEAD) then
        set abilityId = 'AUbu'
    elseif (r == RACE_NIGHTELF) then
        set abilityId = 'AEbu'
    else
        set abilityId = 'ANbu'
    endif
    set bj_lastCreatedCommandButtonEffect = CreateCommandButtonEffect(abilityId, UnitId2String(unitId))
    return bj_lastCreatedCommandButtonEffect
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function GetLastCreatedCommandButtonEffectBJ takes nothing returns commandbuttoneffect
    return bj_lastCreatedCommandButtonEffect
endfunction


//***************************************************************************
//*
//*  Hero and Item Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function GetItemLoc takes item whichItem returns location
    return Location(GetItemX(whichItem), GetItemY(whichItem))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetItemLifeBJ takes widget whichWidget returns real
    return GetWidgetLife(whichWidget)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetItemLifeBJ takes widget whichWidget, real life returns nothing
    call SetWidgetLife(whichWidget, life)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AddHeroXPSwapped takes integer xpToAdd, unit whichHero, boolean showEyeCandy returns nothing
    call AddHeroXP(whichHero, xpToAdd, showEyeCandy)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetHeroLevelBJ takes unit whichHero, integer newLevel, boolean showEyeCandy returns nothing
    local integer oldLevel = GetHeroLevel(whichHero)

    if (newLevel > oldLevel) then
        call SetHeroLevel(whichHero, newLevel, showEyeCandy)
    elseif (newLevel < oldLevel) then
        call UnitStripHeroLevel(whichHero, oldLevel - newLevel)
    else
        // No change in level - ignore the request.
    endif
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function DecUnitAbilityLevelSwapped takes integer abilcode, unit whichUnit returns integer
    return DecUnitAbilityLevel(whichUnit, abilcode)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function IncUnitAbilityLevelSwapped takes integer abilcode, unit whichUnit returns integer
    return IncUnitAbilityLevel(whichUnit, abilcode)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function SetUnitAbilityLevelSwapped takes integer abilcode, unit whichUnit, integer level returns integer
    return SetUnitAbilityLevel(whichUnit, abilcode, level)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function GetUnitAbilityLevelSwapped takes integer abilcode, unit whichUnit returns integer
    return GetUnitAbilityLevel(whichUnit, abilcode)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function UnitHasBuffBJ takes unit whichUnit, integer buffcode returns boolean
    return (GetUnitAbilityLevel(whichUnit, buffcode) > 0)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function UnitRemoveBuffBJ takes integer buffcode, unit whichUnit returns boolean
    return UnitRemoveAbility(whichUnit, buffcode)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitAddItemSwapped takes item whichItem, unit whichHero returns boolean
    return UnitAddItem(whichHero, whichItem)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitAddItemByIdSwapped takes integer itemId, unit whichHero returns item
    // Create the item at the hero's feet first, and then give it to him.
    // This is to ensure that the item will be left at the hero's feet if
    // his inventory is full. 
    set bj_lastCreatedItem = CreateItem(itemId, GetUnitX(whichHero), GetUnitY(whichHero))
    call UnitAddItem(whichHero, bj_lastCreatedItem)
    return bj_lastCreatedItem
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitRemoveItemSwapped takes item whichItem, unit whichHero returns nothing
    set bj_lastRemovedItem = whichItem
    call UnitRemoveItem(whichHero, whichItem)
endfunction

//===========================================================================
// Translates 0-based slot indices to 1-based slot indices.
//

/**
@patch 1.00
*/
function UnitRemoveItemFromSlotSwapped takes integer itemSlot, unit whichHero returns item
    set bj_lastRemovedItem = UnitRemoveItemFromSlot(whichHero, itemSlot-1)
    return bj_lastRemovedItem
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateItemLoc takes integer itemId, location loc returns item
    set bj_lastCreatedItem = CreateItem(itemId, GetLocationX(loc), GetLocationY(loc))
    return bj_lastCreatedItem
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedItem takes nothing returns item
    return bj_lastCreatedItem
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastRemovedItem takes nothing returns item
    return bj_lastRemovedItem
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetItemPositionLoc takes item whichItem, location loc returns nothing
    call SetItemPosition(whichItem, GetLocationX(loc), GetLocationY(loc))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLearnedSkillBJ takes nothing returns integer
    return GetLearnedSkill()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SuspendHeroXPBJ takes boolean flag, unit whichHero returns nothing
    call SuspendHeroXP(whichHero, not flag)
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function SetPlayerHandicapDamageBJ takes player whichPlayer, real handicapPercent returns nothing
    call SetPlayerHandicapDamage(whichPlayer, handicapPercent * 0.01)
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function GetPlayerHandicapDamageBJ takes player whichPlayer returns real
    return GetPlayerHandicapDamage(whichPlayer) * 100
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function SetPlayerHandicapReviveTimeBJ takes player whichPlayer, real handicapPercent returns nothing
    call SetPlayerHandicapReviveTime(whichPlayer, handicapPercent * 0.01)
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function GetPlayerHandicapReviveTimeBJ takes player whichPlayer returns real
    return GetPlayerHandicapReviveTime(whichPlayer) * 100
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerHandicapXPBJ takes player whichPlayer, real handicapPercent returns nothing
    call SetPlayerHandicapXP(whichPlayer, handicapPercent * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetPlayerHandicapXPBJ takes player whichPlayer returns real
    return GetPlayerHandicapXP(whichPlayer) * 100
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerHandicapBJ takes player whichPlayer, real handicapPercent returns nothing
    call SetPlayerHandicap(whichPlayer, handicapPercent * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetPlayerHandicapBJ takes player whichPlayer returns real
    return GetPlayerHandicap(whichPlayer) * 100
endfunction

//===========================================================================

/**
@patch 1.07
*/
function GetHeroStatBJ takes integer whichStat, unit whichHero, boolean includeBonuses returns integer
    if (whichStat == bj_HEROSTAT_STR) then
        return GetHeroStr(whichHero, includeBonuses)
    elseif (whichStat == bj_HEROSTAT_AGI) then
        return GetHeroAgi(whichHero, includeBonuses)
    elseif (whichStat == bj_HEROSTAT_INT) then
        return GetHeroInt(whichHero, includeBonuses)
    else
        // Unrecognized hero stat - return 0
        return 0
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetHeroStat takes unit whichHero, integer whichStat, integer value returns nothing
    // Ignore requests for negative hero stats.
    if (value <= 0) then
        return
    endif

    if (whichStat == bj_HEROSTAT_STR) then
        call SetHeroStr(whichHero, value, true)
    elseif (whichStat == bj_HEROSTAT_AGI) then
        call SetHeroAgi(whichHero, value, true)
    elseif (whichStat == bj_HEROSTAT_INT) then
        call SetHeroInt(whichHero, value, true)
    else
        // Unrecognized hero stat - ignore the request.
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ModifyHeroStat takes integer whichStat, unit whichHero, integer modifyMethod, integer value returns nothing
    if (modifyMethod == bj_MODIFYMETHOD_ADD) then
        call SetHeroStat(whichHero, whichStat, GetHeroStatBJ(whichStat, whichHero, false) + value)
    elseif (modifyMethod == bj_MODIFYMETHOD_SUB) then
        call SetHeroStat(whichHero, whichStat, GetHeroStatBJ(whichStat, whichHero, false) - value)
    elseif (modifyMethod == bj_MODIFYMETHOD_SET) then
        call SetHeroStat(whichHero, whichStat, value)
    else
        // Unrecognized modification method - ignore the request.
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ModifyHeroSkillPoints takes unit whichHero, integer modifyMethod, integer value returns boolean
    if (modifyMethod == bj_MODIFYMETHOD_ADD) then
        return UnitModifySkillPoints(whichHero, value)
    elseif (modifyMethod == bj_MODIFYMETHOD_SUB) then
        return UnitModifySkillPoints(whichHero, -value)
    elseif (modifyMethod == bj_MODIFYMETHOD_SET) then
        return UnitModifySkillPoints(whichHero, value - GetHeroSkillPoints(whichHero))
    else
        // Unrecognized modification method - ignore the request and return failure.
        return false
    endif
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function UnitDropItemPointBJ takes unit whichUnit, item whichItem, real x, real y returns boolean
    return UnitDropItemPoint(whichUnit, whichItem, x, y)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function UnitDropItemPointLoc takes unit whichUnit, item whichItem, location loc returns boolean
    return UnitDropItemPoint(whichUnit, whichItem, GetLocationX(loc), GetLocationY(loc))
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function UnitDropItemSlotBJ takes unit whichUnit, item whichItem, integer slot returns boolean
    return UnitDropItemSlot(whichUnit, whichItem, slot-1)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function UnitDropItemTargetBJ takes unit whichUnit, item whichItem, widget target returns boolean
    return UnitDropItemTarget(whichUnit, whichItem, target)
endfunction

//===========================================================================
// Two distinct trigger actions can't share the same function name, so this
// dummy function simply mimics the behavior of an existing call.
//

/**
@patch 1.00
*/
function UnitUseItemDestructable takes unit whichUnit, item whichItem, widget target returns boolean
    return UnitUseItemTarget(whichUnit, whichItem, target)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitUseItemPointLoc takes unit whichUnit, item whichItem, location loc returns boolean
    return UnitUseItemPoint(whichUnit, whichItem, GetLocationX(loc), GetLocationY(loc))
endfunction

//===========================================================================
// Translates 0-based slot indices to 1-based slot indices.
//

/**
@patch 1.00
*/
function UnitItemInSlotBJ takes unit whichUnit, integer itemSlot returns item
    return UnitItemInSlot(whichUnit, itemSlot-1)
endfunction

//===========================================================================
// Translates 0-based slot indices to 1-based slot indices.
//

/**
@bug Leaks handle `indexItem`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function GetInventoryIndexOfItemTypeBJ takes unit whichUnit, integer itemId returns integer
    local integer index
    local item    indexItem

    set index = 0
    loop
        set indexItem = UnitItemInSlot(whichUnit, index)
        if (indexItem != null) and (GetItemTypeId(indexItem) == itemId) then
            return index + 1
        endif

        set index = index + 1
        exitwhen index >= bj_MAX_INVENTORY
    endloop
    return 0
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetItemOfTypeFromUnitBJ takes unit whichUnit, integer itemId returns item
    local integer index = GetInventoryIndexOfItemTypeBJ(whichUnit, itemId)

    if (index == 0) then
        return null
    else
        return UnitItemInSlot(whichUnit, index - 1)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitHasItemOfTypeBJ takes unit whichUnit, integer itemId returns boolean
    return GetInventoryIndexOfItemTypeBJ(whichUnit, itemId) > 0
endfunction

//===========================================================================

/**
@patch 1.13
*/
function UnitInventoryCount takes unit whichUnit returns integer
    local integer index = 0
    local integer count = 0

    loop
        if (UnitItemInSlot(whichUnit, index) != null) then
            set count = count + 1
        endif

        set index = index + 1
        exitwhen index >= bj_MAX_INVENTORY
    endloop

    return count
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function UnitInventorySizeBJ takes unit whichUnit returns integer
    return UnitInventorySize(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetItemInvulnerableBJ takes item whichItem, boolean flag returns nothing
    call SetItemInvulnerable(whichItem, flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetItemDropOnDeathBJ takes item whichItem, boolean flag returns nothing
    call SetItemDropOnDeath(whichItem, flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetItemDroppableBJ takes item whichItem, boolean flag returns nothing
    call SetItemDroppable(whichItem, flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetItemPlayerBJ takes item whichItem, player whichPlayer, boolean changeColor returns nothing
    call SetItemPlayer(whichItem, whichPlayer, changeColor)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetItemVisibleBJ takes boolean show, item whichItem returns nothing
    call SetItemVisible(whichItem, show)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function IsItemHiddenBJ takes item whichItem returns boolean
    return not IsItemVisible(whichItem)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ChooseRandomItemBJ takes integer level returns integer
    return ChooseRandomItem(level)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ChooseRandomItemExBJ takes integer level, itemtype whichType returns integer
    return ChooseRandomItemEx(whichType, level)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ChooseRandomNPBuildingBJ takes nothing returns integer
    return ChooseRandomNPBuilding()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ChooseRandomCreepBJ takes integer level returns integer
    return ChooseRandomCreep(level)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function EnumItemsInRectBJ takes rect r, code actionFunc returns nothing
    call EnumItemsInRect(r, null, actionFunc)
endfunction

//===========================================================================
// See GroupPickRandomUnitEnum for the details of this algorithm.
//

/**
@patch 1.07
*/
function RandomItemInRectBJEnum takes nothing returns nothing
    set bj_itemRandomConsidered = bj_itemRandomConsidered + 1
    if (GetRandomInt(1, bj_itemRandomConsidered) == 1) then
        set bj_itemRandomCurrentPick = GetEnumItem()
    endif
endfunction

//===========================================================================
// Picks a random item from within a rect, matching a condition
//

/**
@patch 1.07
*/
function RandomItemInRectBJ takes rect r, boolexpr filter returns item
    set bj_itemRandomConsidered = 0
    set bj_itemRandomCurrentPick = null
    call EnumItemsInRect(r, filter, function RandomItemInRectBJEnum)
    call DestroyBoolExpr(filter)
    return bj_itemRandomCurrentPick
endfunction

//===========================================================================
// Picks a random item from within a rect
//

/**
@patch 1.07
*/
function RandomItemInRectSimpleBJ takes rect r returns item
    return RandomItemInRectBJ(r, null)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function CheckItemStatus takes item whichItem, integer status returns boolean
    if (status == bj_ITEM_STATUS_HIDDEN) then
        return not IsItemVisible(whichItem)
    elseif (status == bj_ITEM_STATUS_OWNED) then
        return IsItemOwned(whichItem)
    elseif (status == bj_ITEM_STATUS_INVULNERABLE) then
        return IsItemInvulnerable(whichItem)
    elseif (status == bj_ITEM_STATUS_POWERUP) then
        return IsItemPowerup(whichItem)
    elseif (status == bj_ITEM_STATUS_SELLABLE) then
        return IsItemSellable(whichItem)
    elseif (status == bj_ITEM_STATUS_PAWNABLE) then
        return IsItemPawnable(whichItem)
    else
        // Unrecognized status - return false
        return false
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function CheckItemcodeStatus takes integer itemId, integer status returns boolean
    if (status == bj_ITEMCODE_STATUS_POWERUP) then
        return IsItemIdPowerup(itemId)
    elseif (status == bj_ITEMCODE_STATUS_SELLABLE) then
        return IsItemIdSellable(itemId)
    elseif (status == bj_ITEMCODE_STATUS_PAWNABLE) then
        return IsItemIdPawnable(itemId)
    else
        // Unrecognized status - return false
        return false
    endif
endfunction



//***************************************************************************
//*
//*  Unit Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function UnitId2OrderIdBJ takes integer unitId returns integer
    return unitId
endfunction

//===========================================================================

/**
@patch 1.00
*/
function String2UnitIdBJ takes string unitIdString returns integer
    return UnitId(unitIdString)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitId2StringBJ takes integer unitId returns string
    local string unitString = UnitId2String(unitId)

    if (unitString != null) then
        return unitString
    endif

    // The unitId was not recognized - return an empty string.
    return ""
endfunction

//===========================================================================

/**
@patch 1.00
*/
function String2OrderIdBJ takes string orderIdString returns integer
    local integer orderId
    
    // Check to see if it's a generic order.
    set orderId = OrderId(orderIdString)
    if (orderId != 0) then
        return orderId
    endif

    // Check to see if it's a (train) unit order.
    set orderId = UnitId(orderIdString)
    if (orderId != 0) then
        return orderId
    endif

    // Unrecognized - return 0
    return 0
endfunction

//===========================================================================

/**
@patch 1.00
*/
function OrderId2StringBJ takes integer orderId returns string
    local string orderString

    // Check to see if it's a generic order.
    set orderString = OrderId2String(orderId)
    if (orderString != null) then
        return orderString
    endif

    // Check to see if it's a (train) unit order.
    set orderString = UnitId2String(orderId)
    if (orderString != null) then
        return orderString
    endif

    // Unrecognized - return an empty string.
    return ""
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetIssuedOrderIdBJ takes nothing returns integer
    return GetIssuedOrderId()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetKillingUnitBJ takes nothing returns unit
    return GetKillingUnit()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateUnitAtLocSaveLast takes player id, integer unitid, location loc, real face returns unit
    if (unitid == 'ugol') then
        set bj_lastCreatedUnit = CreateBlightedGoldmine(id, GetLocationX(loc), GetLocationY(loc), face)
    else
        set bj_lastCreatedUnit = CreateUnitAtLoc(id, unitid, loc, face)
    endif

    return bj_lastCreatedUnit
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedUnit takes nothing returns unit
    return bj_lastCreatedUnit
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateNUnitsAtLoc takes integer count, integer unitId, player whichPlayer, location loc, real face returns group
    call GroupClear(bj_lastCreatedGroup)
    loop
        set count = count - 1
        exitwhen count < 0
        call CreateUnitAtLocSaveLast(whichPlayer, unitId, loc, face)
        call GroupAddUnit(bj_lastCreatedGroup, bj_lastCreatedUnit)
    endloop
    return bj_lastCreatedGroup
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateNUnitsAtLocFacingLocBJ takes integer count, integer unitId, player whichPlayer, location loc, location lookAt returns group
    return CreateNUnitsAtLoc(count, unitId, whichPlayer, loc, AngleBetweenPoints(loc, lookAt))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function GetLastCreatedGroupEnum takes nothing returns nothing
    call GroupAddUnit(bj_groupLastCreatedDest, GetEnumUnit())
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedGroup takes nothing returns group
    set bj_groupLastCreatedDest = CreateGroup()
    call ForGroup(bj_lastCreatedGroup, function GetLastCreatedGroupEnum)
    return bj_groupLastCreatedDest
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateCorpseLocBJ takes integer unitid, player whichPlayer, location loc returns unit
    set bj_lastCreatedUnit = CreateCorpse(whichPlayer, unitid, GetLocationX(loc), GetLocationY(loc), GetRandomReal(0, 360))
    return bj_lastCreatedUnit
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitSuspendDecayBJ takes boolean suspend, unit whichUnit returns nothing
    call UnitSuspendDecay(whichUnit, suspend)
endfunction

//===========================================================================

/**
@bug Leaks handle `enumUnit`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function DelayedSuspendDecayStopAnimEnum takes nothing returns nothing
    local unit enumUnit = GetEnumUnit()

    if (GetUnitState(enumUnit, UNIT_STATE_LIFE) <= 0) then
        call SetUnitTimeScale(enumUnit, 0.0001)
    endif
endfunction

//===========================================================================

/**
@bug Leaks handle `enumUnit`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function DelayedSuspendDecayBoneEnum takes nothing returns nothing
    local unit enumUnit = GetEnumUnit()

    if (GetUnitState(enumUnit, UNIT_STATE_LIFE) <= 0) then
        call UnitSuspendDecay(enumUnit, true)
        call SetUnitTimeScale(enumUnit, 0.0001)
    endif
endfunction

//===========================================================================
// Game code explicitly sets the animation back to "decay bone" after the
// initial corpse fades away, so we reset it now.  It's best not to show
// off corpses thus created until after this grace period has passed.
//

/**
@bug Leaks handle `enumUnit`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function DelayedSuspendDecayFleshEnum takes nothing returns nothing
    local unit enumUnit = GetEnumUnit()

    if (GetUnitState(enumUnit, UNIT_STATE_LIFE) <= 0) then
        call UnitSuspendDecay(enumUnit, true)
        call SetUnitTimeScale(enumUnit, 10.0)
        call SetUnitAnimation(enumUnit, "decay flesh")
    endif
endfunction

//===========================================================================
// Waits a short period of time to ensure that the corpse is decaying, and
// then suspend the animation and corpse decay.
//

/**
@bug Leaks handle `boneGroup`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `fleshGroup`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function DelayedSuspendDecay takes nothing returns nothing
    local group boneGroup
    local group fleshGroup

    // Switch the global unit groups over to local variables and recreate
    // the global versions, so that this function can handle overlapping
    // calls.
    set boneGroup = bj_suspendDecayBoneGroup
    set fleshGroup = bj_suspendDecayFleshGroup
    set bj_suspendDecayBoneGroup = CreateGroup()
    set bj_suspendDecayFleshGroup = CreateGroup()

    call ForGroup(fleshGroup, function DelayedSuspendDecayStopAnimEnum)
    call ForGroup(boneGroup, function DelayedSuspendDecayStopAnimEnum)

    call TriggerSleepAction(bj_CORPSE_MAX_DEATH_TIME)
    call ForGroup(fleshGroup, function DelayedSuspendDecayFleshEnum)
    call ForGroup(boneGroup, function DelayedSuspendDecayBoneEnum)

    call TriggerSleepAction(0.05)
    call ForGroup(fleshGroup, function DelayedSuspendDecayStopAnimEnum)

    call DestroyGroup(boneGroup)
    call DestroyGroup(fleshGroup)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function DelayedSuspendDecayCreate takes nothing returns nothing
    set bj_delayedSuspendDecayTrig = CreateTrigger()
    call TriggerRegisterTimerExpireEvent(bj_delayedSuspendDecayTrig, bj_delayedSuspendDecayTimer)
    call TriggerAddAction(bj_delayedSuspendDecayTrig, function DelayedSuspendDecay)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function CreatePermanentCorpseLocBJ takes integer style, integer unitid, player whichPlayer, location loc, real facing returns unit
    set bj_lastCreatedUnit = CreateCorpse(whichPlayer, unitid, GetLocationX(loc), GetLocationY(loc), facing)
    call SetUnitBlendTime(bj_lastCreatedUnit, 0)

    if (style == bj_CORPSETYPE_FLESH) then
        call SetUnitAnimation(bj_lastCreatedUnit, "decay flesh")
        call GroupAddUnit(bj_suspendDecayFleshGroup, bj_lastCreatedUnit)
    elseif (style == bj_CORPSETYPE_BONE) then
        call SetUnitAnimation(bj_lastCreatedUnit, "decay bone")
        call GroupAddUnit(bj_suspendDecayBoneGroup, bj_lastCreatedUnit)
    else
        // Unknown decay style - treat as skeletal.
        call SetUnitAnimation(bj_lastCreatedUnit, "decay bone")
        call GroupAddUnit(bj_suspendDecayBoneGroup, bj_lastCreatedUnit)
    endif

    call TimerStart(bj_delayedSuspendDecayTimer, 0.05, false, null)
    return bj_lastCreatedUnit
endfunction

//===========================================================================

/**
The arguments are swapped for WorldEdit usage. Equivalent to `GetUnitState`.

@patch 1.00
*/
function GetUnitStateSwap takes unitstate whichState, unit whichUnit returns real
    return GetUnitState(whichUnit, whichState)
endfunction

//===========================================================================

/**
Returns the current unit state in percent.
Unit states representing current value and max value must be provided in that order.

In case of failure returns 0.

**Example:** a unit with 30/60 HP will return 50.0, meaning the unit is at 50% HP.

@patch 1.07
*/
function GetUnitStatePercent takes unit whichUnit, unitstate whichState, unitstate whichMaxState returns real
    local real value    = GetUnitState(whichUnit, whichState)
    local real maxValue = GetUnitState(whichUnit, whichMaxState)

    // Return 0 for null units.
    if (whichUnit == null) or (maxValue == 0) then
        return 0.0
    endif

    return value / maxValue * 100.0
endfunction

//===========================================================================

/**
@patch 1.07
*/
function GetUnitLifePercent takes unit whichUnit returns real
    return GetUnitStatePercent(whichUnit, UNIT_STATE_LIFE, UNIT_STATE_MAX_LIFE)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function GetUnitManaPercent takes unit whichUnit returns real
    return GetUnitStatePercent(whichUnit, UNIT_STATE_MANA, UNIT_STATE_MAX_MANA)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SelectUnitSingle takes unit whichUnit returns nothing
    call ClearSelection()
    call SelectUnit(whichUnit, true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SelectGroupBJEnum takes nothing returns nothing
    call SelectUnit( GetEnumUnit(), true )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SelectGroupBJ takes group g returns nothing
    call ClearSelection()
    call ForGroup( g, function SelectGroupBJEnum )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SelectUnitAdd takes unit whichUnit returns nothing
    call SelectUnit(whichUnit, true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SelectUnitRemove takes unit whichUnit returns nothing
    call SelectUnit(whichUnit, false)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ClearSelectionForPlayer takes player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ClearSelection()
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SelectUnitForPlayerSingle takes unit whichUnit, player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ClearSelection()
        call SelectUnit(whichUnit, true)
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SelectGroupForPlayerBJ takes group g, player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ClearSelection()
        call ForGroup( g, function SelectGroupBJEnum )
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SelectUnitAddForPlayer takes unit whichUnit, player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SelectUnit(whichUnit, true)
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SelectUnitRemoveForPlayer takes unit whichUnit, player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SelectUnit(whichUnit, false)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitLifeBJ takes unit whichUnit, real newValue returns nothing
    call SetUnitState(whichUnit, UNIT_STATE_LIFE, RMaxBJ(0,newValue))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitManaBJ takes unit whichUnit, real newValue returns nothing
    call SetUnitState(whichUnit, UNIT_STATE_MANA, RMaxBJ(0,newValue))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitLifePercentBJ takes unit whichUnit, real percent returns nothing
    call SetUnitState(whichUnit, UNIT_STATE_LIFE, GetUnitState(whichUnit, UNIT_STATE_MAX_LIFE) * RMaxBJ(0,percent) * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitManaPercentBJ takes unit whichUnit, real percent returns nothing
    call SetUnitState(whichUnit, UNIT_STATE_MANA, GetUnitState(whichUnit, UNIT_STATE_MAX_MANA) * RMaxBJ(0,percent) * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitDeadBJ takes unit whichUnit returns boolean
    return GetUnitState(whichUnit, UNIT_STATE_LIFE) <= 0
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitAliveBJ takes unit whichUnit returns boolean
    return not IsUnitDeadBJ(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitGroupDeadBJEnum takes nothing returns nothing
    if not IsUnitDeadBJ(GetEnumUnit()) then
        set bj_isUnitGroupDeadResult = false
    endif
endfunction

//===========================================================================
// Returns true if every unit of the group is dead.
//

/**
@patch 1.00
*/
function IsUnitGroupDeadBJ takes group g returns boolean
    // If the user wants the group destroyed, remember that fact and clear
    // the flag, in case it is used again in the callback.
    local boolean wantDestroy = bj_wantDestroyGroup
    set bj_wantDestroyGroup = false

    set bj_isUnitGroupDeadResult = true
    call ForGroup(g, function IsUnitGroupDeadBJEnum)

    // If the user wants the group destroyed, do so now.
    if (wantDestroy) then
        call DestroyGroup(g)
    endif
    return bj_isUnitGroupDeadResult
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitGroupEmptyBJEnum takes nothing returns nothing
    set bj_isUnitGroupEmptyResult = false
endfunction

//===========================================================================
// Returns true if the group contains no units.
//

/**
@patch 1.00
*/
function IsUnitGroupEmptyBJ takes group g returns boolean
    // If the user wants the group destroyed, remember that fact and clear
    // the flag, in case it is used again in the callback.
    local boolean wantDestroy = bj_wantDestroyGroup
    set bj_wantDestroyGroup = false

    set bj_isUnitGroupEmptyResult = true
    call ForGroup(g, function IsUnitGroupEmptyBJEnum)

    // If the user wants the group destroyed, do so now.
    if (wantDestroy) then
        call DestroyGroup(g)
    endif
    return bj_isUnitGroupEmptyResult
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitGroupInRectBJEnum takes nothing returns nothing
    if not RectContainsUnit(bj_isUnitGroupInRectRect, GetEnumUnit()) then
        set bj_isUnitGroupInRectResult = false
    endif
endfunction

//===========================================================================
// Returns true if every unit of the group is within the given rect.
//

/**
@patch 1.00
*/
function IsUnitGroupInRectBJ takes group g, rect r returns boolean
    set bj_isUnitGroupInRectResult = true
    set bj_isUnitGroupInRectRect = r
    call ForGroup(g, function IsUnitGroupInRectBJEnum)
    return bj_isUnitGroupInRectResult
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitHiddenBJ takes unit whichUnit returns boolean
    return IsUnitHidden(whichUnit)
endfunction

//===========================================================================

/**
Hides unit.

Equivalent to `ShowUnit(whichUnit, false)`

@param whichUnit Target unit to hide

@patch 1.00
*/
function ShowUnitHide takes unit whichUnit returns nothing
    call ShowUnit(whichUnit, false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ShowUnitShow takes unit whichUnit returns nothing
    // Prevent dead heroes from being unhidden.
    if (IsUnitType(whichUnit, UNIT_TYPE_HERO) and IsUnitDeadBJ(whichUnit)) then
        return
    endif

    call ShowUnit(whichUnit, true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IssueHauntOrderAtLocBJFilter takes nothing returns boolean
    return GetUnitTypeId(GetFilterUnit()) == 'ngol'
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `goldMine`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function IssueHauntOrderAtLocBJ takes unit whichPeon, location loc returns boolean
    local group g = null
    local unit goldMine = null

    // Search for a gold mine within a 1-cell radius of the specified location.
    set g = CreateGroup()
    call GroupEnumUnitsInRangeOfLoc(g, loc, 2*bj_CELLWIDTH, filterIssueHauntOrderAtLocBJ)
    set goldMine = FirstOfGroup(g)
    call DestroyGroup(g)

    // If no mine was found, abort the request.
    if (goldMine == null) then
        return false
    endif

    // Issue the Haunt Gold Mine order.
    return IssueTargetOrderById(whichPeon, 'ugol', goldMine)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IssueBuildOrderByIdLocBJ takes unit whichPeon, integer unitId, location loc returns boolean
    if (unitId == 'ugol') then
        return IssueHauntOrderAtLocBJ(whichPeon, loc)
    else
        return IssueBuildOrderById(whichPeon, unitId, GetLocationX(loc), GetLocationY(loc))
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IssueTrainOrderByIdBJ takes unit whichUnit, integer unitId returns boolean
    return IssueImmediateOrderById(whichUnit, unitId)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GroupTrainOrderByIdBJ takes group g, integer unitId returns boolean
    return GroupImmediateOrderById(g, unitId)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IssueUpgradeOrderByIdBJ takes unit whichUnit, integer techId returns boolean
    return IssueImmediateOrderById(whichUnit, techId)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetAttackedUnitBJ takes nothing returns unit
    return GetTriggerUnit()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitFlyHeightBJ takes unit whichUnit, real newHeight, real rate returns nothing
    call SetUnitFlyHeight(whichUnit, newHeight, rate)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitTurnSpeedBJ takes unit whichUnit, real turnSpeed returns nothing
    call SetUnitTurnSpeed(whichUnit, turnSpeed)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitPropWindowBJ takes unit whichUnit, real propWindow returns nothing
    local real angle = propWindow
    if (angle <= 0) then
        set angle = 1
    elseif (angle >= 360) then
        set angle = 359
    endif
    set angle = angle * bj_DEGTORAD

    call SetUnitPropWindow(whichUnit, angle)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetUnitPropWindowBJ takes unit whichUnit returns real
    return GetUnitPropWindow(whichUnit) * bj_RADTODEG
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetUnitDefaultPropWindowBJ takes unit whichUnit returns real
    return GetUnitDefaultPropWindow(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitBlendTimeBJ takes unit whichUnit, real blendTime returns nothing
    call SetUnitBlendTime(whichUnit, blendTime)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitAcquireRangeBJ takes unit whichUnit, real acquireRange returns nothing
    call SetUnitAcquireRange(whichUnit, acquireRange)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitSetCanSleepBJ takes unit whichUnit, boolean canSleep returns nothing
    call UnitAddSleep(whichUnit, canSleep)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitCanSleepBJ takes unit whichUnit returns boolean
    return UnitCanSleep(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitWakeUpBJ takes unit whichUnit returns nothing
    call UnitWakeUp(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitIsSleepingBJ takes unit whichUnit returns boolean
    return UnitIsSleeping(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function WakePlayerUnitsEnum takes nothing returns nothing
    call UnitWakeUp(GetEnumUnit())
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function WakePlayerUnits takes player whichPlayer returns nothing
    local group g = CreateGroup()
    call GroupEnumUnitsOfPlayer(g, whichPlayer, null)
    call ForGroup(g, function WakePlayerUnitsEnum)
    call DestroyGroup(g)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function EnableCreepSleepBJ takes boolean enable returns nothing
    call SetPlayerState(Player(PLAYER_NEUTRAL_AGGRESSIVE), PLAYER_STATE_NO_CREEP_SLEEP, IntegerTertiaryOp(enable, 0, 1))

    // If we're disabling, attempt to wake any already-sleeping creeps.
    if (not enable) then
        call WakePlayerUnits(Player(PLAYER_NEUTRAL_AGGRESSIVE))
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function UnitGenerateAlarms takes unit whichUnit, boolean generate returns boolean
    return UnitIgnoreAlarm(whichUnit, not generate)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function DoesUnitGenerateAlarms takes unit whichUnit returns boolean
    return not UnitIgnoreAlarmToggled(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PauseAllUnitsBJEnum takes nothing returns nothing
    call PauseUnit( GetEnumUnit(), bj_pauseAllUnitsFlag )
endfunction

//===========================================================================
// Pause all units 

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function PauseAllUnitsBJ takes boolean pause returns nothing
    local integer index
    local player  indexPlayer
    local group   g

    set bj_pauseAllUnitsFlag = pause
    set g = CreateGroup()
    set index = 0
    loop
        set indexPlayer = Player( index )

        // If this is a computer slot, pause/resume the AI.
        if (GetPlayerController( indexPlayer ) == MAP_CONTROL_COMPUTER) then
            call PauseCompAI( indexPlayer, pause )
        endif

        // Enumerate and unpause every unit owned by the player.
        call GroupEnumUnitsOfPlayer( g, indexPlayer, null )
        call ForGroup( g, function PauseAllUnitsBJEnum )
        call GroupClear( g )

        set index = index + 1
        exitwhen index == bj_MAX_PLAYER_SLOTS
    endloop
    call DestroyGroup(g)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PauseUnitBJ takes boolean pause, unit whichUnit returns nothing
    call PauseUnit(whichUnit, pause)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitPausedBJ takes unit whichUnit returns boolean
    return IsUnitPaused(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function UnitPauseTimedLifeBJ takes boolean flag, unit whichUnit returns nothing
    call UnitPauseTimedLife(whichUnit, flag)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function UnitApplyTimedLifeBJ takes real duration, integer buffId, unit whichUnit returns nothing
    call UnitApplyTimedLife(whichUnit, buffId, duration)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitShareVisionBJ takes boolean share, unit whichUnit, player whichPlayer returns nothing
    call UnitShareVision(whichUnit, whichPlayer, share)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnitRemoveBuffsBJ takes integer buffType, unit whichUnit returns nothing
    if (buffType == bj_REMOVEBUFFS_POSITIVE) then
        call UnitRemoveBuffs(whichUnit, true, false)
    elseif (buffType == bj_REMOVEBUFFS_NEGATIVE) then
        call UnitRemoveBuffs(whichUnit, false, true)
    elseif (buffType == bj_REMOVEBUFFS_ALL) then
        call UnitRemoveBuffs(whichUnit, true, true)
    elseif (buffType == bj_REMOVEBUFFS_NONTLIFE) then
        call UnitRemoveBuffsEx(whichUnit, true, true, false, false, false, true, false)
    else
        // Unrecognized dispel type - ignore the request.
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function UnitRemoveBuffsExBJ takes integer polarity, integer resist, unit whichUnit, boolean bTLife, boolean bAura returns nothing
    local boolean bPos   = (polarity == bj_BUFF_POLARITY_EITHER) or (polarity == bj_BUFF_POLARITY_POSITIVE)
    local boolean bNeg   = (polarity == bj_BUFF_POLARITY_EITHER) or (polarity == bj_BUFF_POLARITY_NEGATIVE)
    local boolean bMagic = (resist == bj_BUFF_RESIST_BOTH) or (resist == bj_BUFF_RESIST_MAGIC)
    local boolean bPhys  = (resist == bj_BUFF_RESIST_BOTH) or (resist == bj_BUFF_RESIST_PHYSICAL)

    call UnitRemoveBuffsEx(whichUnit, bPos, bNeg, bMagic, bPhys, bTLife, bAura, false)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function UnitCountBuffsExBJ takes integer polarity, integer resist, unit whichUnit, boolean bTLife, boolean bAura returns integer
    local boolean bPos   = (polarity == bj_BUFF_POLARITY_EITHER) or (polarity == bj_BUFF_POLARITY_POSITIVE)
    local boolean bNeg   = (polarity == bj_BUFF_POLARITY_EITHER) or (polarity == bj_BUFF_POLARITY_NEGATIVE)
    local boolean bMagic = (resist == bj_BUFF_RESIST_BOTH) or (resist == bj_BUFF_RESIST_MAGIC)
    local boolean bPhys  = (resist == bj_BUFF_RESIST_BOTH) or (resist == bj_BUFF_RESIST_PHYSICAL)

    return UnitCountBuffsEx(whichUnit, bPos, bNeg, bMagic, bPhys, bTLife, bAura, false)
endfunction

//===========================================================================

/**
Equivalent to `UnitRemoveAbility`, but arguments are swapped.

@patch 1.00
*/
function UnitRemoveAbilityBJ takes integer abilityId, unit whichUnit returns boolean
    return UnitRemoveAbility(whichUnit, abilityId)
endfunction

//===========================================================================

/**
Equivalent to `UnitAddAbility`, but arguments are swapped.

@patch 1.07
*/
function UnitAddAbilityBJ takes integer abilityId, unit whichUnit returns boolean
    return UnitAddAbility(whichUnit, abilityId)
endfunction

//===========================================================================

/**
@patch 1.15
*/
function UnitRemoveTypeBJ takes unittype whichType, unit whichUnit returns boolean
    return UnitRemoveType(whichUnit, whichType)
endfunction

//===========================================================================

/**
@patch 1.15
*/
function UnitAddTypeBJ takes unittype whichType, unit whichUnit returns boolean
    return UnitAddType(whichUnit, whichType)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function UnitMakeAbilityPermanentBJ takes boolean permanent, integer abilityId, unit whichUnit returns boolean
    return UnitMakeAbilityPermanent(whichUnit, permanent, abilityId)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitExplodedBJ takes unit whichUnit, boolean exploded returns nothing
    call SetUnitExploded(whichUnit, exploded)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ExplodeUnitBJ takes unit whichUnit returns nothing
    call SetUnitExploded(whichUnit, true)
    call KillUnit(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetTransportUnitBJ takes nothing returns unit
    return GetTransportUnit()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLoadedUnitBJ takes nothing returns unit
    return GetLoadedUnit()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitInTransportBJ takes unit whichUnit, unit whichTransport returns boolean
    return IsUnitInTransport(whichUnit, whichTransport)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitLoadedBJ takes unit whichUnit returns boolean
    return IsUnitLoaded(whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsUnitIllusionBJ takes unit whichUnit returns boolean
    return IsUnitIllusion(whichUnit)
endfunction

//===========================================================================
// This attempts to replace a unit with a new unit type by creating a new
// unit of the desired type using the old unit's location, facing, etc.
//

/**
@bug Leaks handles:

1. `oldUnit`
1. `newUnit`
1. `indexItem`

In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function ReplaceUnitBJ takes unit whichUnit, integer newUnitId, integer unitStateMethod returns unit
    local unit    oldUnit = whichUnit
    local unit    newUnit
    local boolean wasHidden
    local integer index
    local item    indexItem
    local real    oldRatio

    // If we have bogus data, don't attempt the replace.
    if (oldUnit == null) then
        set bj_lastReplacedUnit = oldUnit
        return oldUnit
    endif

    // Hide the original unit.
    set wasHidden = IsUnitHidden(oldUnit)
    call ShowUnit(oldUnit, false)

    // Create the replacement unit.
    if (newUnitId == 'ugol') then
        set newUnit = CreateBlightedGoldmine(GetOwningPlayer(oldUnit), GetUnitX(oldUnit), GetUnitY(oldUnit), GetUnitFacing(oldUnit))
    else
        set newUnit = CreateUnit(GetOwningPlayer(oldUnit), newUnitId, GetUnitX(oldUnit), GetUnitY(oldUnit), GetUnitFacing(oldUnit))
    endif

    // Set the unit's life and mana according to the requested method.
    if (unitStateMethod == bj_UNIT_STATE_METHOD_RELATIVE) then
        // Set the replacement's current/max life ratio to that of the old unit.
        // If both units have mana, do the same for mana.
        if (GetUnitState(oldUnit, UNIT_STATE_MAX_LIFE) > 0) then
            set oldRatio = GetUnitState(oldUnit, UNIT_STATE_LIFE) / GetUnitState(oldUnit, UNIT_STATE_MAX_LIFE)
            call SetUnitState(newUnit, UNIT_STATE_LIFE, oldRatio * GetUnitState(newUnit, UNIT_STATE_MAX_LIFE))
        endif

        if (GetUnitState(oldUnit, UNIT_STATE_MAX_MANA) > 0) and (GetUnitState(newUnit, UNIT_STATE_MAX_MANA) > 0) then
            set oldRatio = GetUnitState(oldUnit, UNIT_STATE_MANA) / GetUnitState(oldUnit, UNIT_STATE_MAX_MANA)
            call SetUnitState(newUnit, UNIT_STATE_MANA, oldRatio * GetUnitState(newUnit, UNIT_STATE_MAX_MANA))
        endif
    elseif (unitStateMethod == bj_UNIT_STATE_METHOD_ABSOLUTE) then
        // Set the replacement's current life to that of the old unit.
        // If the new unit has mana, do the same for mana.
        call SetUnitState(newUnit, UNIT_STATE_LIFE, GetUnitState(oldUnit, UNIT_STATE_LIFE))
        if (GetUnitState(newUnit, UNIT_STATE_MAX_MANA) > 0) then
            call SetUnitState(newUnit, UNIT_STATE_MANA, GetUnitState(oldUnit, UNIT_STATE_MANA))
        endif
    elseif (unitStateMethod == bj_UNIT_STATE_METHOD_DEFAULTS) then
        // The newly created unit should already have default life and mana.
    elseif (unitStateMethod == bj_UNIT_STATE_METHOD_MAXIMUM) then
        // Use max life and mana.
        call SetUnitState(newUnit, UNIT_STATE_LIFE, GetUnitState(newUnit, UNIT_STATE_MAX_LIFE))
        call SetUnitState(newUnit, UNIT_STATE_MANA, GetUnitState(newUnit, UNIT_STATE_MAX_MANA))
    else
        // Unrecognized unit state method - ignore the request.
    endif

    // Mirror properties of the old unit onto the new unit.
    //call PauseUnit(newUnit, IsUnitPaused(oldUnit))
    call SetResourceAmount(newUnit, GetResourceAmount(oldUnit))

    // If both the old and new units are heroes, handle their hero info.
    if (IsUnitType(oldUnit, UNIT_TYPE_HERO) and IsUnitType(newUnit, UNIT_TYPE_HERO)) then
        call SetHeroXP(newUnit, GetHeroXP(oldUnit), false)

        set index = 0
        loop
            set indexItem = UnitItemInSlot(oldUnit, index)
            if (indexItem != null) then
                call UnitRemoveItem(oldUnit, indexItem)
                call UnitAddItem(newUnit, indexItem)
            endif

            set index = index + 1
            exitwhen index >= bj_MAX_INVENTORY
        endloop
    endif

    // Remove or kill the original unit.  It is sometimes unsafe to remove
    // hidden units, so kill the original unit if it was previously hidden.
    if wasHidden then
        call KillUnit(oldUnit)
        call RemoveUnit(oldUnit)
    else
        call RemoveUnit(oldUnit)
    endif

    set bj_lastReplacedUnit = newUnit
    return newUnit
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastReplacedUnitBJ takes nothing returns unit
    return bj_lastReplacedUnit
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitPositionLocFacingBJ takes unit whichUnit, location loc, real facing returns nothing
    call SetUnitPositionLoc(whichUnit, loc)
    call SetUnitFacing(whichUnit, facing)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitPositionLocFacingLocBJ takes unit whichUnit, location loc, location lookAt returns nothing
    call SetUnitPositionLoc(whichUnit, loc)
    call SetUnitFacing(whichUnit, AngleBetweenPoints(loc, lookAt))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function AddItemToStockBJ takes integer itemId, unit whichUnit, integer currentStock, integer stockMax returns nothing
    call AddItemToStock(whichUnit, itemId, currentStock, stockMax)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function AddUnitToStockBJ takes integer unitId, unit whichUnit, integer currentStock, integer stockMax returns nothing
    call AddUnitToStock(whichUnit, unitId, currentStock, stockMax)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RemoveItemFromStockBJ takes integer itemId, unit whichUnit returns nothing
    call RemoveItemFromStock(whichUnit, itemId)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RemoveUnitFromStockBJ takes integer unitId, unit whichUnit returns nothing
    call RemoveUnitFromStock(whichUnit, unitId)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetUnitUseFoodBJ takes boolean enable, unit whichUnit returns nothing
    call SetUnitUseFood(whichUnit, enable)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function UnitDamagePointLoc takes unit whichUnit, real delay, real radius, location loc, real amount, attacktype whichAttack, damagetype whichDamage returns boolean
    return UnitDamagePoint(whichUnit, delay, radius, GetLocationX(loc), GetLocationY(loc), amount, true, false, whichAttack, whichDamage, WEAPON_TYPE_WHOKNOWS)
endfunction

//===========================================================================

/**
@patch 1.17a
*/
function UnitDamageTargetBJ takes unit whichUnit, unit target, real amount, attacktype whichAttack, damagetype whichDamage returns boolean
    return UnitDamageTarget(whichUnit, target, amount, true, false, whichAttack, whichDamage, WEAPON_TYPE_WHOKNOWS)
endfunction



//***************************************************************************
//*
//*  Destructable Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function CreateDestructableLoc takes integer objectid, location loc, real facing, real scale, integer variation returns destructable
    set bj_lastCreatedDestructable = CreateDestructable(objectid, GetLocationX(loc), GetLocationY(loc), facing, scale, variation)
    return bj_lastCreatedDestructable
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateDeadDestructableLocBJ takes integer objectid, location loc, real facing, real scale, integer variation returns destructable
    set bj_lastCreatedDestructable = CreateDeadDestructable(objectid, GetLocationX(loc), GetLocationY(loc), facing, scale, variation)
    return bj_lastCreatedDestructable
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedDestructable takes nothing returns destructable
    return bj_lastCreatedDestructable
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ShowDestructableBJ takes boolean flag, destructable d returns nothing
    call ShowDestructable(d, flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDestructableInvulnerableBJ takes destructable d, boolean flag returns nothing
    call SetDestructableInvulnerable(d, flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsDestructableInvulnerableBJ takes destructable d returns boolean
    return IsDestructableInvulnerable(d)
endfunction

//===========================================================================

/**
Returns a new location of destructable's position.

@patch 1.00
*/
function GetDestructableLoc takes destructable whichDestructable returns location
    return Location(GetDestructableX(whichDestructable), GetDestructableY(whichDestructable))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function EnumDestructablesInRectAll takes rect r, code actionFunc returns nothing
    call EnumDestructablesInRect(r, null, actionFunc)
endfunction

//===========================================================================

/**
@bug Leaks handle `destLoc`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function EnumDestructablesInCircleBJFilter takes nothing returns boolean
    local location destLoc = GetDestructableLoc(GetFilterDestructable())
    local boolean result

    set result = DistanceBetweenPoints(destLoc, bj_enumDestructableCenter) <= bj_enumDestructableRadius
    call RemoveLocation(destLoc)
    return result
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsDestructableDeadBJ takes destructable d returns boolean
    return GetDestructableLife(d) <= 0
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsDestructableAliveBJ takes destructable d returns boolean
    return not IsDestructableDeadBJ(d)
endfunction

//===========================================================================
// See GroupPickRandomUnitEnum for the details of this algorithm.
//

/**
@patch 1.00
*/
function RandomDestructableInRectBJEnum takes nothing returns nothing
    set bj_destRandomConsidered = bj_destRandomConsidered + 1
    if (GetRandomInt(1,bj_destRandomConsidered) == 1) then
        set bj_destRandomCurrentPick = GetEnumDestructable()
    endif
endfunction

//===========================================================================
// Picks a random destructable from within a rect, matching a condition
//

/**
@note Destroys the `filter` `boolexpr` received as argument.

@patch 1.00
*/
function RandomDestructableInRectBJ takes rect r, boolexpr filter returns destructable
    set bj_destRandomConsidered = 0
    set bj_destRandomCurrentPick = null
    call EnumDestructablesInRect(r, filter, function RandomDestructableInRectBJEnum)
    call DestroyBoolExpr(filter)
    return bj_destRandomCurrentPick
endfunction

//===========================================================================
// Picks a random destructable from within a rect
//

/**
@patch 1.00
*/
function RandomDestructableInRectSimpleBJ takes rect r returns destructable
    return RandomDestructableInRectBJ(r, null)
endfunction

//===========================================================================
// Enumerates within a rect, with a filter to narrow the enumeration down
// objects within a circular area.
//

/**
@bug Leaks handle `r`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function EnumDestructablesInCircleBJ takes real radius, location loc, code actionFunc returns nothing
    local rect r

    if (radius >= 0) then
        set bj_enumDestructableCenter = loc
        set bj_enumDestructableRadius = radius
        set r = GetRectFromCircleBJ(loc, radius)
        call EnumDestructablesInRect(r, filterEnumDestructablesInCircleBJ, actionFunc)
        call RemoveRect(r)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDestructableLifePercentBJ takes destructable d, real percent returns nothing
    call SetDestructableLife(d, GetDestructableMaxLife(d) * percent * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDestructableMaxLifeBJ takes destructable d, real max returns nothing
    call SetDestructableMaxLife(d, max)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ModifyGateBJ takes integer gateOperation, destructable d returns nothing
    if (gateOperation == bj_GATEOPERATION_CLOSE) then
        if (GetDestructableLife(d) <= 0) then
            call DestructableRestoreLife(d, GetDestructableMaxLife(d), true)
        endif
        call SetDestructableAnimation(d, "stand")
    elseif (gateOperation == bj_GATEOPERATION_OPEN) then
        if (GetDestructableLife(d) > 0) then
            call KillDestructable(d)
        endif
        call SetDestructableAnimation(d, "death alternate")
    elseif (gateOperation == bj_GATEOPERATION_DESTROY) then
        if (GetDestructableLife(d) > 0) then
            call KillDestructable(d)
        endif
        call SetDestructableAnimation(d, "death")
    else
        // Unrecognized gate state - ignore the request.
    endif
endfunction

//===========================================================================
// Determine the elevator's height from its occlusion height.
//

/**
@patch 1.07
*/
function GetElevatorHeight takes destructable d returns integer
    local integer height

    set height = 1 + R2I(GetDestructableOccluderHeight(d) / bj_CLIFFHEIGHT)
    if (height < 1) or (height > 3) then
        set height = 1
    endif
    return height
endfunction

//===========================================================================
// To properly animate an elevator, we must know not only what height we
// want to change to, but also what height we are currently at.  This code
// determines the elevator's current height from its occlusion height.
// Arbitrarily changing an elevator's occlusion height is thus inadvisable.
//

/**
@patch 1.07
*/
function ChangeElevatorHeight takes destructable d, integer newHeight returns nothing
    local integer oldHeight

    // Cap the new height within the supported range.
    set newHeight = IMaxBJ(1, newHeight)
    set newHeight = IMinBJ(3, newHeight)

    // Find out what height the elevator is already at.
    set oldHeight = GetElevatorHeight(d)

    // Set the elevator's occlusion height.
    call SetDestructableOccluderHeight(d, bj_CLIFFHEIGHT*(newHeight-1))

    if (newHeight == 1) then
        if (oldHeight == 2) then
            call SetDestructableAnimation(d, "birth")
            call QueueDestructableAnimation(d, "stand")
        elseif (oldHeight == 3) then
            call SetDestructableAnimation(d, "birth third")
            call QueueDestructableAnimation(d, "stand")
        else
            // Unrecognized old height - snap to new height.
            call SetDestructableAnimation(d, "stand")
        endif
    elseif (newHeight == 2) then
        if (oldHeight == 1) then
            call SetDestructableAnimation(d, "death")
            call QueueDestructableAnimation(d, "stand second")
        elseif (oldHeight == 3) then
            call SetDestructableAnimation(d, "birth second")
            call QueueDestructableAnimation(d, "stand second")
        else
            // Unrecognized old height - snap to new height.
            call SetDestructableAnimation(d, "stand second")
        endif
    elseif (newHeight == 3) then
        if (oldHeight == 1) then
            call SetDestructableAnimation(d, "death third")
            call QueueDestructableAnimation(d, "stand third")
        elseif (oldHeight == 2) then
            call SetDestructableAnimation(d, "death second")
            call QueueDestructableAnimation(d, "stand third")
        else
            // Unrecognized old height - snap to new height.
            call SetDestructableAnimation(d, "stand third")
        endif
    else
        // Unrecognized new height - ignore the request.
    endif
endfunction

//===========================================================================
// Grab the unit and throw his own coords in his face, forcing him to push
// and shove until he finds a spot where noone will bother him.
//

/**
@bug Leaks handle `nudgee`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function NudgeUnitsInRectEnum takes nothing returns nothing
    local unit nudgee = GetEnumUnit()

    call SetUnitPosition(nudgee, GetUnitX(nudgee), GetUnitY(nudgee))
endfunction

//===========================================================================

/**
@bug Leaks handle `nudgee`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function NudgeItemsInRectEnum takes nothing returns nothing
    local item nudgee = GetEnumItem()

    call SetItemPosition(nudgee, GetItemX(nudgee), GetItemY(nudgee))
endfunction

//===========================================================================
// Nudge the items and units within a given rect ever so gently, so as to
// encourage them to find locations where they can peacefully coexist with
// pathing restrictions and live happy, fruitful lives.
//

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function NudgeObjectsInRect takes rect nudgeArea returns nothing
    local group        g

    set g = CreateGroup()
    call GroupEnumUnitsInRect(g, nudgeArea, null)
    call ForGroup(g, function NudgeUnitsInRectEnum)
    call DestroyGroup(g)

    call EnumItemsInRect(nudgeArea, null, function NudgeItemsInRectEnum)
endfunction

//===========================================================================

/**
@bug Leaks handle `d`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function NearbyElevatorExistsEnum takes nothing returns nothing
    local destructable d     = GetEnumDestructable()
    local integer      dType = GetDestructableTypeId(d)

    if (dType == bj_ELEVATOR_CODE01) or (dType == bj_ELEVATOR_CODE02) then
        set bj_elevatorNeighbor = d
    endif
endfunction

//===========================================================================

/**
@bug Leaks handle `r`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function NearbyElevatorExists takes real x, real y returns boolean
    local real findThreshold = 32
    local rect r

    // If another elevator is overlapping this one, ignore the wall.
    set r = Rect(x - findThreshold, y - findThreshold, x + findThreshold, y + findThreshold)
    set bj_elevatorNeighbor = null
    call EnumDestructablesInRect(r, null, function NearbyElevatorExistsEnum)
    call RemoveRect(r)

    return bj_elevatorNeighbor != null
endfunction

//===========================================================================

/**
@patch 1.07
*/
function FindElevatorWallBlockerEnum takes nothing returns nothing
    set bj_elevatorWallBlocker = GetEnumDestructable()
endfunction

//===========================================================================
// This toggles pathing on or off for one wall of an elevator by killing
// or reviving a pathing blocker at the appropriate location (and creating
// the pathing blocker in the first place, if it does not yet exist).
//

/**
@bug Leaks handle `blocker`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `r`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function ChangeElevatorWallBlocker takes real x, real y, real facing, boolean open returns nothing
    local destructable blocker = null
    local real         findThreshold = 32
    local real         nudgeLength   = 4.25 * bj_CELLWIDTH
    local real         nudgeWidth    = 1.25 * bj_CELLWIDTH
    local rect         r

    // Search for the pathing blocker within the general area.
    set r = Rect(x - findThreshold, y - findThreshold, x + findThreshold, y + findThreshold)
    set bj_elevatorWallBlocker = null
    call EnumDestructablesInRect(r, null, function FindElevatorWallBlockerEnum)
    call RemoveRect(r)
    set blocker = bj_elevatorWallBlocker

    // Ensure that the blocker exists.
    if (blocker == null) then
        set blocker = CreateDeadDestructable(bj_ELEVATOR_BLOCKER_CODE, x, y, facing, 1, 0)
    elseif (GetDestructableTypeId(blocker) != bj_ELEVATOR_BLOCKER_CODE) then
        // If a different destructible exists in the blocker's spot, ignore
        // the request.  (Two destructibles cannot occupy the same location
        // on the map, so we cannot create an elevator blocker here.)
        return
    endif

    if (open) then
        // Ensure that the blocker is dead.
        if (GetDestructableLife(blocker) > 0) then
            call KillDestructable(blocker)
        endif
    else
        // Ensure that the blocker is alive.
        if (GetDestructableLife(blocker) <= 0) then
            call DestructableRestoreLife(blocker, GetDestructableMaxLife(blocker), false)
        endif

        // Nudge any objects standing in the blocker's way.
        if (facing == 0) then
            set r = Rect(x - nudgeWidth/2, y - nudgeLength/2, x + nudgeWidth/2, y + nudgeLength/2)
            call NudgeObjectsInRect(r)
            call RemoveRect(r)
        elseif (facing == 90) then
            set r = Rect(x - nudgeLength/2, y - nudgeWidth/2, x + nudgeLength/2, y + nudgeWidth/2)
            call NudgeObjectsInRect(r)
            call RemoveRect(r)
        else
            // Unrecognized blocker angle - don't nudge anything.
        endif
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ChangeElevatorWalls takes boolean open, integer walls, destructable d returns nothing
    local real x = GetDestructableX(d)
    local real y = GetDestructableY(d)
    local real distToBlocker = 192
    local real distToNeighbor = 256

    if (walls == bj_ELEVATOR_WALL_TYPE_ALL) or (walls == bj_ELEVATOR_WALL_TYPE_EAST) then
        if (not NearbyElevatorExists(x + distToNeighbor, y)) then
            call ChangeElevatorWallBlocker(x + distToBlocker, y, 0, open)
        endif
    endif

    if (walls == bj_ELEVATOR_WALL_TYPE_ALL) or (walls == bj_ELEVATOR_WALL_TYPE_NORTH) then
        if (not NearbyElevatorExists(x, y + distToNeighbor)) then
            call ChangeElevatorWallBlocker(x, y + distToBlocker, 90, open)
        endif
    endif

    if (walls == bj_ELEVATOR_WALL_TYPE_ALL) or (walls == bj_ELEVATOR_WALL_TYPE_SOUTH) then
        if (not NearbyElevatorExists(x, y - distToNeighbor)) then
            call ChangeElevatorWallBlocker(x, y - distToBlocker, 90, open)
        endif
    endif

    if (walls == bj_ELEVATOR_WALL_TYPE_ALL) or (walls == bj_ELEVATOR_WALL_TYPE_WEST) then
        if (not NearbyElevatorExists(x - distToNeighbor, y)) then
            call ChangeElevatorWallBlocker(x - distToBlocker, y, 0, open)
        endif
    endif
endfunction



//***************************************************************************
//*
//*  Neutral Building Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function WaygateActivateBJ takes boolean activate, unit waygate returns nothing
    call WaygateActivate(waygate, activate)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function WaygateIsActiveBJ takes unit waygate returns boolean
    return WaygateIsActive(waygate)
endfunction

//===========================================================================

/**
@note The caller must not forget to remove the passed `loc` location.

@patch 1.00
*/
function WaygateSetDestinationLocBJ takes unit waygate, location loc returns nothing
    call WaygateSetDestination(waygate, GetLocationX(loc), GetLocationY(loc))
endfunction

//===========================================================================

/**
@note Creates a new location object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function WaygateGetDestinationLocBJ takes unit waygate returns location
    return Location(WaygateGetDestinationX(waygate), WaygateGetDestinationY(waygate))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function UnitSetUsesAltIconBJ takes boolean flag, unit whichUnit returns nothing
    call UnitSetUsesAltIcon(whichUnit, flag)
endfunction



//***************************************************************************
//*
//*  UI Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
Only call `ForceUIKey` locally for whichPlayer. Since this emulates a player's actions, it cannot desync, like if player pressed a keyboard button.

@note See: `ForceUICancel`, `ForceUICancelBJ`

@patch 1.00
*/
function ForceUIKeyBJ takes player whichPlayer, string key returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ForceUIKey(key)
    endif
endfunction

//===========================================================================

/**
Only call `ForceUICancel` locally for whichPlayer. Since this emulates a player's actions, it cannot desync, like if player pressed a keyboard button.

@note See: `ForceUIKey`, `ForceUIKeyBJ`

@patch 1.00
*/
function ForceUICancelBJ takes player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ForceUICancel()
    endif
endfunction



//***************************************************************************
//*
//*  Group and Force Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@note If the global `bj_wantDestroyGroup` is set to `true` then destroys the passed group `whichGroup`.

@patch 1.07
*/
function ForGroupBJ takes group whichGroup, code callback returns nothing
    // If the user wants the group destroyed, remember that fact and clear
    // the flag, in case it is used again in the callback.
    local boolean wantDestroy = bj_wantDestroyGroup
    set bj_wantDestroyGroup = false

    call ForGroup(whichGroup, callback)

    // If the user wants the group destroyed, do so now.
    if (wantDestroy) then
        call DestroyGroup(whichGroup)
    endif
endfunction

//===========================================================================

/**
See `GroupAddUnit` (native). Here only the argument order is swapped for WorldEdit trigger usage.

@patch 1.00
*/
function GroupAddUnitSimple takes unit whichUnit, group whichGroup returns nothing
    call GroupAddUnit(whichGroup, whichUnit)
endfunction

//===========================================================================

/**
See `GroupRemoveUnit` (native). Here only the argument order is swapped for WorldEdit trigger usage.

@patch 1.00
*/
function GroupRemoveUnitSimple takes unit whichUnit, group whichGroup returns nothing
    call GroupRemoveUnit(whichGroup, whichUnit)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GroupAddGroupEnum takes nothing returns nothing
    call GroupAddUnit(bj_groupAddGroupDest, GetEnumUnit())
endfunction

//===========================================================================

/**
@note If the global `bj_wantDestroyGroup` is set to `true` then destroys the passed group `sourceGroup`.

@patch 1.00
*/
function GroupAddGroup takes group sourceGroup, group destGroup returns nothing
    // If the user wants the group destroyed, remember that fact and clear
    // the flag, in case it is used again in the callback.
    local boolean wantDestroy = bj_wantDestroyGroup
    set bj_wantDestroyGroup = false

    set bj_groupAddGroupDest = destGroup
    call ForGroup(sourceGroup, function GroupAddGroupEnum)

    // If the user wants the group destroyed, do so now.
    if (wantDestroy) then
        call DestroyGroup(sourceGroup)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GroupRemoveGroupEnum takes nothing returns nothing
    call GroupRemoveUnit(bj_groupRemoveGroupDest, GetEnumUnit())
endfunction

//===========================================================================

/**
@note If the global `bj_wantDestroyGroup` is set to `true` then destroys the passed group `sourceGroup`.

@patch 1.00
*/
function GroupRemoveGroup takes group sourceGroup, group destGroup returns nothing
    // If the user wants the group destroyed, remember that fact and clear
    // the flag, in case it is used again in the callback.
    local boolean wantDestroy = bj_wantDestroyGroup
    set bj_wantDestroyGroup = false

    set bj_groupRemoveGroupDest = destGroup
    call ForGroup(sourceGroup, function GroupRemoveGroupEnum)

    // If the user wants the group destroyed, do so now.
    if (wantDestroy) then
        call DestroyGroup(sourceGroup)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ForceAddPlayerSimple takes player whichPlayer, force whichForce returns nothing
    call ForceAddPlayer(whichForce, whichPlayer)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ForceRemovePlayerSimple takes player whichPlayer, force whichForce returns nothing
    call ForceRemovePlayer(whichForce, whichPlayer)
endfunction

//===========================================================================
// Consider each unit, one at a time, keeping a "current pick".   Once all units
// are considered, this "current pick" will be the resulting random unit.
//
// The chance of picking a given unit over the "current pick" is 1/N, where N is
// the number of units considered thusfar (including the current consideration).
//

/**
@patch 1.00
*/
function GroupPickRandomUnitEnum takes nothing returns nothing
    set bj_groupRandomConsidered = bj_groupRandomConsidered + 1
    if (GetRandomInt(1,bj_groupRandomConsidered) == 1) then
        set bj_groupRandomCurrentPick = GetEnumUnit()
    endif
endfunction

//===========================================================================
// Picks a random unit from a group.
//

/**
@note If the global `bj_wantDestroyGroup` is set to `true` then destroys the passed group `whichGroup`.

@patch 1.00
*/
function GroupPickRandomUnit takes group whichGroup returns unit
    // If the user wants the group destroyed, remember that fact and clear
    // the flag, in case it is used again in the callback.
    local boolean wantDestroy = bj_wantDestroyGroup
    set bj_wantDestroyGroup = false

    set bj_groupRandomConsidered = 0
    set bj_groupRandomCurrentPick = null
    call ForGroup(whichGroup, function GroupPickRandomUnitEnum)

    // If the user wants the group destroyed, do so now.
    if (wantDestroy) then
        call DestroyGroup(whichGroup)
    endif
    return bj_groupRandomCurrentPick
endfunction

//===========================================================================
// See GroupPickRandomUnitEnum for the details of this algorithm.
//

/**
@patch 1.00
*/
function ForcePickRandomPlayerEnum takes nothing returns nothing
    set bj_forceRandomConsidered = bj_forceRandomConsidered + 1
    if (GetRandomInt(1,bj_forceRandomConsidered) == 1) then
        set bj_forceRandomCurrentPick = GetEnumPlayer()
    endif
endfunction

//===========================================================================
// Picks a random player from a force.
//

/**
@patch 1.00
*/
function ForcePickRandomPlayer takes force whichForce returns player
    set bj_forceRandomConsidered = 0
    set bj_forceRandomCurrentPick = null
    call ForForce(whichForce, function ForcePickRandomPlayerEnum)
    return bj_forceRandomCurrentPick
endfunction

//===========================================================================

/**
For the target player choose all currently selected units that match the filter and run `enumAction` on them.

@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Destroys the `enumFilter` `boolexpr` received as argument.

@patch 1.00
*/
function EnumUnitsSelected takes player whichPlayer, boolexpr enumFilter, code enumAction returns nothing
    local group g = CreateGroup()
    call SyncSelections()
    call GroupEnumUnitsSelected(g, whichPlayer, enumFilter)
    call DestroyBoolExpr(enumFilter)
    call ForGroup(g, enumAction)
    call DestroyGroup(g)
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Destroys the `filter` `boolexpr` received as argument.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsInRectMatching takes rect r, boolexpr filter returns group
    local group g = CreateGroup()
    call GroupEnumUnitsInRect(g, r, filter)
    call DestroyBoolExpr(filter)
    return g
endfunction

//===========================================================================

/**
@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsInRectAll takes rect r returns group
    return GetUnitsInRectMatching(r, null)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetUnitsInRectOfPlayerFilter takes nothing returns boolean
    return GetOwningPlayer(GetFilterUnit()) == bj_groupEnumOwningPlayer
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsInRectOfPlayer takes rect r, player whichPlayer returns group
    local group g = CreateGroup()
    set bj_groupEnumOwningPlayer = whichPlayer
    call GroupEnumUnitsInRect(g, r, filterGetUnitsInRectOfPlayer)
    return g
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsInRangeOfLocMatching takes real radius, location whichLocation, boolexpr filter returns group
    local group g = CreateGroup()
    call GroupEnumUnitsInRangeOfLoc(g, whichLocation, radius, filter)
    call DestroyBoolExpr(filter)
    return g
endfunction

//===========================================================================

/**
@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsInRangeOfLocAll takes real radius, location whichLocation returns group
    return GetUnitsInRangeOfLocMatching(radius, whichLocation, null)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetUnitsOfTypeIdAllFilter takes nothing returns boolean
    return GetUnitTypeId(GetFilterUnit()) == bj_groupEnumTypeId
endfunction

//===========================================================================

/**
@bug Leaks handle `result`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsOfTypeIdAll takes integer unitid returns group
    local group   result = CreateGroup()
    local group   g      = CreateGroup()
    local integer index

    set index = 0
    loop
        set bj_groupEnumTypeId = unitid
        call GroupClear(g)
        call GroupEnumUnitsOfPlayer(g, Player(index), filterGetUnitsOfTypeIdAll)
        call GroupAddGroup(g, result)

        set index = index + 1
        exitwhen index == bj_MAX_PLAYER_SLOTS
    endloop
    call DestroyGroup(g)

    return result
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Destroys the `filter` `boolexpr` received as argument.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsOfPlayerMatching takes player whichPlayer, boolexpr filter returns group
    local group g = CreateGroup()
    call GroupEnumUnitsOfPlayer(g, whichPlayer, filter)
    call DestroyBoolExpr(filter)
    return g
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetUnitsOfPlayerAll takes player whichPlayer returns group
    return GetUnitsOfPlayerMatching(whichPlayer, null)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetUnitsOfPlayerAndTypeIdFilter takes nothing returns boolean
    return GetUnitTypeId(GetFilterUnit()) == bj_groupEnumTypeId
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsOfPlayerAndTypeId takes player whichPlayer, integer unitid returns group
    local group g = CreateGroup()
    set bj_groupEnumTypeId = unitid
    call GroupEnumUnitsOfPlayer(g, whichPlayer, filterGetUnitsOfPlayerAndTypeId)
    return g
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetUnitsSelectedAll takes player whichPlayer returns group
    local group g = CreateGroup()
    call SyncSelections()
    call GroupEnumUnitsSelected(g, whichPlayer, null)
    return g
endfunction

//===========================================================================

/**
Creates a new force to include the target player.

@bug Leaks handle `f`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new force object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetForceOfPlayer takes player whichPlayer returns force
    local force f = CreateForce()
    call ForceAddPlayer(f, whichPlayer)
    return f
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetPlayersAll takes nothing returns force
    return bj_FORCE_ALL_PLAYERS
endfunction

//===========================================================================

/**
@bug Leaks handle `f`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new force object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetPlayersByMapControl takes mapcontrol whichControl returns force
    local force f = CreateForce()
    local integer playerIndex
    local player  indexPlayer

    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        if GetPlayerController(indexPlayer) == whichControl then
            call ForceAddPlayer(f, indexPlayer)
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYER_SLOTS
    endloop

    return f
endfunction

//===========================================================================

/**
@bug Leaks handle `f`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new force object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetPlayersAllies takes player whichPlayer returns force
    local force f = CreateForce()
    call ForceEnumAllies(f, whichPlayer, null)
    return f
endfunction

//===========================================================================

/**
@bug Leaks handle `f`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new force object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetPlayersEnemies takes player whichPlayer returns force
    local force f = CreateForce()
    call ForceEnumEnemies(f, whichPlayer, null)
    return f
endfunction

//===========================================================================

/**
@bug Leaks handle `f`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Destroys the `filter` `boolexpr` received as argument.

@note Creates a new force object and returns it. The caller must remove it on its own after use.

@patch 1.00
*/
function GetPlayersMatching takes boolexpr filter returns force
    local force f = CreateForce()
    call ForceEnumPlayers(f, filter)
    call DestroyBoolExpr(filter)
    return f
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CountUnitsInGroupEnum takes nothing returns nothing
    set bj_groupCountUnits = bj_groupCountUnits + 1
endfunction

//===========================================================================

/**
@note If the global `bj_wantDestroyGroup` is set to `true` then destroys the passed group `g`.

@patch 1.00
*/
function CountUnitsInGroup takes group g returns integer
    // If the user wants the group destroyed, remember that fact and clear
    // the flag, in case it is used again in the callback.
    local boolean wantDestroy = bj_wantDestroyGroup
    set bj_wantDestroyGroup = false

    set bj_groupCountUnits = 0
    call ForGroup(g, function CountUnitsInGroupEnum)

    // If the user wants the group destroyed, do so now.
    if (wantDestroy) then
        call DestroyGroup(g)
    endif
    return bj_groupCountUnits
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CountPlayersInForceEnum takes nothing returns nothing
    set bj_forceCountPlayers = bj_forceCountPlayers + 1
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CountPlayersInForceBJ takes force f returns integer
    set bj_forceCountPlayers = 0
    call ForForce(f, function CountPlayersInForceEnum)
    return bj_forceCountPlayers
endfunction

//===========================================================================

/**
@bug Not an even distribution. See <http://www.hiveworkshop.com/forums/l-715/g-275344/>.

@patch 1.07
*/
function GetRandomSubGroupEnum takes nothing returns nothing
    if (bj_randomSubGroupWant > 0) then
        if (bj_randomSubGroupWant >= bj_randomSubGroupTotal) or (GetRandomReal(0,1) < bj_randomSubGroupChance) then
            // We either need every remaining unit, or the unit passed its chance check.
            call GroupAddUnit(bj_randomSubGroupGroup, GetEnumUnit())
            set bj_randomSubGroupWant = bj_randomSubGroupWant - 1
        endif
    endif
    set bj_randomSubGroupTotal = bj_randomSubGroupTotal - 1
endfunction

//===========================================================================

/**
@bug Not an even distribution. See <http://www.hiveworkshop.com/forums/l-715/g-275344/>.

@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@note Creates a new group object and returns it. The caller must remove it on its own after use.

@patch 1.07
*/
function GetRandomSubGroup takes integer count, group sourceGroup returns group
    local group g = CreateGroup()

    set bj_randomSubGroupGroup = g
    set bj_randomSubGroupWant  = count
    set bj_randomSubGroupTotal = CountUnitsInGroup(sourceGroup)

    if (bj_randomSubGroupWant <= 0 or bj_randomSubGroupTotal <= 0) then
        return g
    endif

    set bj_randomSubGroupChance = I2R(bj_randomSubGroupWant) / I2R(bj_randomSubGroupTotal)
    call ForGroup(sourceGroup, function GetRandomSubGroupEnum)
    return g
endfunction

//===========================================================================

/**
@bug Leaks handle `filterUnit`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function LivingPlayerUnitsOfTypeIdFilter takes nothing returns boolean
    local unit filterUnit = GetFilterUnit()
    return IsUnitAliveBJ(filterUnit) and GetUnitTypeId(filterUnit) == bj_livingPlayerUnitsTypeId
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function CountLivingPlayerUnitsOfTypeId takes integer unitId, player whichPlayer returns integer
    local group g
    local integer matchedCount

    set g = CreateGroup()
    set bj_livingPlayerUnitsTypeId = unitId
    call GroupEnumUnitsOfPlayer(g, whichPlayer, filterLivingPlayerUnitsOfTypeId)
    set matchedCount = CountUnitsInGroup(g)
    call DestroyGroup(g)

    return matchedCount
endfunction



//***************************************************************************
//*
//*  Animation Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function ResetUnitAnimation takes unit whichUnit returns nothing
    call SetUnitAnimation(whichUnit, "stand")
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitTimeScalePercent takes unit whichUnit, real percentScale returns nothing
    call SetUnitTimeScale(whichUnit, percentScale * 0.01)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUnitScalePercent takes unit whichUnit, real percentScaleX, real percentScaleY, real percentScaleZ returns nothing
    call SetUnitScale(whichUnit, percentScaleX * 0.01, percentScaleY * 0.01, percentScaleZ * 0.01)
endfunction

//===========================================================================
// This version differs from the common.j interface in that the alpha value
// is reversed so as to be displayed as transparency, and all four parameters
// are treated as percentages rather than bytes.
//

/**
Sets the unit's color to the color defined by (red,green,blue,alpha).

@param whichUnit The unit which will be colored.

@param red An integer from 0-100 determining the amount of red color.

@param green An integer from 0-100 determining the amount of green color.

@param blue An integer from 0-100 determining the amount of blue color.

@param transparency An integer from 0-100 determining the transparency. A value of 100 is complete transparency while a value of 0 is complete opacity.

@patch 1.00
*/
function SetUnitVertexColorBJ takes unit whichUnit, real red, real green, real blue, real transparency returns nothing
    call SetUnitVertexColor(whichUnit, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
Adds a blinking circle around the unit with the color (red,green,blue,alpha).
The circle blinks twice. This function is commonly used for cinematic modes
and is seen in `TransmissionFromUnitWithNameBJ`.

@param whichUnit The unit the indicator will be applied to.

@param red An integer from 0-100 determining the amount of red color in the indicator.

@param green An integer from 0-100 determining the amount of green color in the indicator.

@param blue An integer from 0-100 determining the amount of blue color in the indicator.

@param transparency An integer from 0-100 determining the transparency of the indicator. A value of 100 is complete transparency while a value of 0 is complete opacity.

@note The size of the indicator depends on a unit's selection size. To modify
this, you must edit the object editor field of the unit listed as "Art - Selection Size".

@patch 1.00
*/
function UnitAddIndicatorBJ takes unit whichUnit, real red, real green, real blue, real transparency returns nothing
    call AddIndicator(whichUnit, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
Adds a blinking circle around the destructable with the color (red,green,blue,alpha).
The circle blinks twice. This function is commonly used for cinematic modes
and is seen in `TransmissionFromUnitWithNameBJ`.

@param whichDestructable The destructable the indicator will be applied to.

@param red An integer from 0-100 determining the amount of red color in the indicator.

@param green An integer from 0-100 determining the amount of green color in the indicator.

@param blue An integer from 0-100 determining the amount of blue color in the indicator.

@param transparency An integer from 0-100 determining the transparency of the indicator. A value of 100 is complete transparency while a value of 0 is complete opacity.

@note The size of the indicator depends on a destructable's selection size. To modify
this, you must edit the object editor field of the destructable listed as "Art - Selection Size".

@patch 1.00
*/
function DestructableAddIndicatorBJ takes destructable whichDestructable, real red, real green, real blue, real transparency returns nothing
    call AddIndicator(whichDestructable, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
Adds a blinking circle around the item with the color (red,green,blue,alpha).
The circle blinks twice. This function is commonly used for cinematic modes
and is seen in `TransmissionFromUnitWithNameBJ`.

@param whichItem The item the indicator will be applied to.

@param red An integer from 0-100 determining the amount of red color in the indicator.

@param green An integer from 0-100 determining the amount of green color in the indicator.

@param blue An integer from 0-100 determining the amount of blue color in the indicator.

@param transparency An integer from 0-100 determining the transparency of the indicator. A value of 100 is complete transparency while a value of 0 is complete opacity.

@note The size of the indicator depends on a item's selection size. To modify
this, you must edit the object editor field of the item listed as "Art - Selection Size".

@patch 1.00
*/
function ItemAddIndicatorBJ takes item whichItem, real red, real green, real blue, real transparency returns nothing
    call AddIndicator(whichItem, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================
// Sets a unit's facing to point directly at a location.
//

/**
@bug Leaks handle `unitLoc`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function SetUnitFacingToFaceLocTimed takes unit whichUnit, location target, real duration returns nothing
    local location unitLoc = GetUnitLoc(whichUnit)

    call SetUnitFacingTimed(whichUnit, AngleBetweenPoints(unitLoc, target), duration)
    call RemoveLocation(unitLoc)
endfunction

//===========================================================================
// Sets a unit's facing to point directly at another unit.
//

/**
@bug Leaks handle `unitLoc`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function SetUnitFacingToFaceUnitTimed takes unit whichUnit, unit target, real duration returns nothing
    local location unitLoc = GetUnitLoc(target)

    call SetUnitFacingToFaceLocTimed(whichUnit, unitLoc, duration)
    call RemoveLocation(unitLoc)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QueueUnitAnimationBJ takes unit whichUnit, string whichAnimation returns nothing
    call QueueUnitAnimation(whichUnit, whichAnimation)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDestructableAnimationBJ takes destructable d, string whichAnimation returns nothing
    call SetDestructableAnimation(d, whichAnimation)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QueueDestructableAnimationBJ takes destructable d, string whichAnimation returns nothing
    call QueueDestructableAnimation(d, whichAnimation)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetDestAnimationSpeedPercent takes destructable d, real percentScale returns nothing
    call SetDestructableAnimationSpeed(d, percentScale * 0.01)
endfunction



//***************************************************************************
//*
//*  Dialog Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function DialogDisplayBJ takes boolean flag, dialog whichDialog, player whichPlayer returns nothing
    call DialogDisplay(whichPlayer, whichDialog, flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DialogSetMessageBJ takes dialog whichDialog, string message returns nothing
    call DialogSetMessage(whichDialog, message)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DialogAddButtonBJ takes dialog whichDialog, string buttonText returns button
    set bj_lastCreatedButton = DialogAddButton(whichDialog, buttonText,0)
    return bj_lastCreatedButton
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DialogAddButtonWithHotkeyBJ takes dialog whichDialog, string buttonText, integer hotkey returns button
    set bj_lastCreatedButton = DialogAddButton(whichDialog, buttonText,hotkey)
    return bj_lastCreatedButton
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DialogClearBJ takes dialog whichDialog returns nothing
    call DialogClear(whichDialog)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedButtonBJ takes nothing returns button
    return bj_lastCreatedButton
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetClickedButtonBJ takes nothing returns button
    return GetClickedButton()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetClickedDialogBJ takes nothing returns dialog
    return GetClickedDialog()
endfunction



//***************************************************************************
//*
//*  Alliance Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerAllianceBJ takes player sourcePlayer, alliancetype whichAllianceSetting, boolean value, player otherPlayer returns nothing
    // Prevent players from attempting to ally with themselves.
    if (sourcePlayer == otherPlayer) then
        return
    endif

    call SetPlayerAlliance(sourcePlayer, otherPlayer, whichAllianceSetting, value)
endfunction

//===========================================================================
// Set all flags used by the in-game "Ally" checkbox.
//

/**
@patch 1.00
*/
function SetPlayerAllianceStateAllyBJ takes player sourcePlayer, player otherPlayer, boolean flag returns nothing
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_PASSIVE,       flag)
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_HELP_REQUEST,  flag)
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_HELP_RESPONSE, flag)
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_SHARED_XP,     flag)
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_SHARED_SPELLS, flag)
endfunction

//===========================================================================
// Set all flags used by the in-game "Shared Vision" checkbox.
//

/**
@patch 1.00
*/
function SetPlayerAllianceStateVisionBJ takes player sourcePlayer, player otherPlayer, boolean flag returns nothing
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_SHARED_VISION, flag)
endfunction

//===========================================================================
// Set all flags used by the in-game "Shared Units" checkbox.
//

/**
@patch 1.00
*/
function SetPlayerAllianceStateControlBJ takes player sourcePlayer, player otherPlayer, boolean flag returns nothing
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_SHARED_CONTROL, flag)
endfunction

//===========================================================================
// Set all flags used by the in-game "Shared Units" checkbox with the Full
// Shared Unit Control feature enabled.
//

/**
@patch 1.00
*/
function SetPlayerAllianceStateFullControlBJ takes player sourcePlayer, player otherPlayer, boolean flag returns nothing
    call SetPlayerAlliance(sourcePlayer, otherPlayer, ALLIANCE_SHARED_ADVANCED_CONTROL, flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerAllianceStateBJ takes player sourcePlayer, player otherPlayer, integer allianceState returns nothing
    // Prevent players from attempting to ally with themselves.
    if (sourcePlayer == otherPlayer) then
        return
    endif

    if allianceState == bj_ALLIANCE_UNALLIED then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, false )
    elseif allianceState == bj_ALLIANCE_UNALLIED_VISION then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, false )
    elseif allianceState == bj_ALLIANCE_ALLIED then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, false )
    elseif allianceState == bj_ALLIANCE_ALLIED_VISION then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, false )
    elseif allianceState == bj_ALLIANCE_ALLIED_UNITS then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, false )
    elseif allianceState == bj_ALLIANCE_ALLIED_ADVUNITS then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, true  )
    elseif allianceState == bj_ALLIANCE_NEUTRAL then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, false )
        call SetPlayerAlliance( sourcePlayer, otherPlayer, ALLIANCE_PASSIVE, true )
    elseif allianceState == bj_ALLIANCE_NEUTRAL_VISION then
        call SetPlayerAllianceStateAllyBJ(        sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateVisionBJ(      sourcePlayer, otherPlayer, true  )
        call SetPlayerAllianceStateControlBJ(     sourcePlayer, otherPlayer, false )
        call SetPlayerAllianceStateFullControlBJ( sourcePlayer, otherPlayer, false )
        call SetPlayerAlliance( sourcePlayer, otherPlayer, ALLIANCE_PASSIVE, true )
    else
        // Unrecognized alliance state - ignore the request.
    endif
endfunction

//===========================================================================
// Set the alliance states for an entire force towards another force.
//

/**
@patch 1.07
*/
function SetForceAllianceStateBJ takes force sourceForce, force targetForce, integer allianceState returns nothing
    local integer sourceIndex
    local integer targetIndex

    set sourceIndex = 0
    loop

        if (sourceForce==bj_FORCE_ALL_PLAYERS or IsPlayerInForce(Player(sourceIndex), sourceForce)) then
            set targetIndex = 0
            loop
                if (targetForce==bj_FORCE_ALL_PLAYERS or IsPlayerInForce(Player(targetIndex), targetForce)) then
                    call SetPlayerAllianceStateBJ(Player(sourceIndex), Player(targetIndex), allianceState)
                endif

                set targetIndex = targetIndex + 1
                exitwhen targetIndex == bj_MAX_PLAYER_SLOTS
            endloop
        endif

        set sourceIndex = sourceIndex + 1
        exitwhen sourceIndex == bj_MAX_PLAYER_SLOTS
    endloop
endfunction

//===========================================================================
// Test to see if two players are co-allied (allied with each other).
//

/**
@patch 1.00
*/
function PlayersAreCoAllied takes player playerA, player playerB returns boolean
    // Players are considered to be allied with themselves.
    if (playerA == playerB) then
        return true
    endif

    // Co-allies are both allied with each other.
    if GetPlayerAlliance(playerA, playerB, ALLIANCE_PASSIVE) then
        if GetPlayerAlliance(playerB, playerA, ALLIANCE_PASSIVE) then
            return true
        endif
    endif
    return false
endfunction

//===========================================================================
// Force (whichPlayer) AI player to share vision and advanced unit control 
// with all AI players of its allies.
//

/**
@patch 1.00
*/
function ShareEverythingWithTeamAI takes player whichPlayer returns nothing
    local integer playerIndex
    local player  indexPlayer

    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        if (PlayersAreCoAllied(whichPlayer, indexPlayer) and whichPlayer != indexPlayer) then
            if (GetPlayerController(indexPlayer) == MAP_CONTROL_COMPUTER) then
                call SetPlayerAlliance(whichPlayer, indexPlayer, ALLIANCE_SHARED_VISION, true)
                call SetPlayerAlliance(whichPlayer, indexPlayer, ALLIANCE_SHARED_CONTROL, true)
                call SetPlayerAlliance(whichPlayer, indexPlayer, ALLIANCE_SHARED_ADVANCED_CONTROL, true)
            endif
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================
// Force (whichPlayer) to share vision and advanced unit control with all of his/her allies.
//

/**
@patch 1.00
*/
function ShareEverythingWithTeam takes player whichPlayer returns nothing
    local integer playerIndex
    local player  indexPlayer

    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        if (PlayersAreCoAllied(whichPlayer, indexPlayer) and whichPlayer != indexPlayer) then
            call SetPlayerAlliance(whichPlayer, indexPlayer, ALLIANCE_SHARED_VISION, true)
            call SetPlayerAlliance(whichPlayer, indexPlayer, ALLIANCE_SHARED_CONTROL, true)
            call SetPlayerAlliance(indexPlayer, whichPlayer, ALLIANCE_SHARED_CONTROL, true)
            call SetPlayerAlliance(whichPlayer, indexPlayer, ALLIANCE_SHARED_ADVANCED_CONTROL, true)
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================
// Creates a 'Neutral Victim' player slot.  This slot is passive towards all
// other players, but all other players are aggressive towards him/her.
// 

/**
@patch 1.00
*/
function ConfigureNeutralVictim takes nothing returns nothing
    local integer index
    local player indexPlayer
    local player neutralVictim = Player(bj_PLAYER_NEUTRAL_VICTIM)

    set index = 0
    loop
        set indexPlayer = Player(index)

        call SetPlayerAlliance(neutralVictim, indexPlayer, ALLIANCE_PASSIVE, true)
        call SetPlayerAlliance(indexPlayer, neutralVictim, ALLIANCE_PASSIVE, false)

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop

    // Neutral Victim and Neutral Aggressive should not fight each other.
    set indexPlayer = Player(PLAYER_NEUTRAL_AGGRESSIVE)
    call SetPlayerAlliance(neutralVictim, indexPlayer, ALLIANCE_PASSIVE, true)
    call SetPlayerAlliance(indexPlayer, neutralVictim, ALLIANCE_PASSIVE, true)

    // Neutral Victim does not give bounties.
    call SetPlayerState(neutralVictim, PLAYER_STATE_GIVES_BOUNTY, 0)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MakeUnitsPassiveForPlayerEnum takes nothing returns nothing
    call SetUnitOwner(GetEnumUnit(), Player(bj_PLAYER_NEUTRAL_VICTIM), false)
endfunction

//===========================================================================
// Change ownership for every unit of (whichPlayer)'s team to neutral passive.
//

/**
@patch 1.00
*/
function MakeUnitsPassiveForPlayer takes player whichPlayer returns nothing
    local group   playerUnits = CreateGroup()
    call CachePlayerHeroData(whichPlayer)
    call GroupEnumUnitsOfPlayer(playerUnits, whichPlayer, null)
    call ForGroup(playerUnits, function MakeUnitsPassiveForPlayerEnum)
    call DestroyGroup(playerUnits)
endfunction

//===========================================================================
// Change ownership for every unit of (whichPlayer)'s team to neutral passive.
//

/**
@patch 1.00
*/
function MakeUnitsPassiveForTeam takes player whichPlayer returns nothing
    local integer playerIndex
    local player  indexPlayer

    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        if PlayersAreCoAllied(whichPlayer, indexPlayer) then
            call MakeUnitsPassiveForPlayer(indexPlayer)
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================
// Determine whether or not victory/defeat is disabled via cheat codes.
//

/**
@patch 1.00
*/
function AllowVictoryDefeat takes playergameresult gameResult returns boolean
    if (gameResult == PLAYER_GAME_RESULT_VICTORY) then
        return not IsNoVictoryCheat()
    endif
    if (gameResult == PLAYER_GAME_RESULT_DEFEAT) then
        return not IsNoDefeatCheat()
    endif
    if (gameResult == PLAYER_GAME_RESULT_NEUTRAL) then
        return (not IsNoVictoryCheat()) and (not IsNoDefeatCheat())
    endif
    return true
endfunction

//===========================================================================

/**
@patch 1.00
*/
function EndGameBJ takes nothing returns nothing
    call EndGame( true )
endfunction

//===========================================================================

/**
@bug Leaks handle `t`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `d`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeVictoryDialogBJ takes player whichPlayer, boolean leftGame returns nothing
    local trigger t = CreateTrigger()
    local dialog  d = DialogCreate()
    local string formatString

    // Display "player was victorious" or "player has left the game" message
    if (leftGame) then
        set formatString = GetLocalizedString( "PLAYER_LEFT_GAME" )
    else
        set formatString = GetLocalizedString( "PLAYER_VICTORIOUS" )
    endif

    call DisplayTimedTextFromPlayer(whichPlayer, 0, 0, 60, formatString)

    call DialogSetMessage( d, GetLocalizedString( "GAMEOVER_VICTORY_MSG" ) )
    call DialogAddButton( d, GetLocalizedString( "GAMEOVER_CONTINUE_GAME" ), GetLocalizedHotkey("GAMEOVER_CONTINUE_GAME") )

    set t = CreateTrigger()
    call TriggerRegisterDialogButtonEvent( t, DialogAddQuitButton( d, true, GetLocalizedString( "GAMEOVER_QUIT_GAME" ), GetLocalizedHotkey("GAMEOVER_QUIT_GAME") ) )

    call DialogDisplay( whichPlayer, d, true )
    call StartSoundForPlayerBJ( whichPlayer, bj_victoryDialogSound )
endfunction

//===========================================================================

/**
@bug Leaks handle `t`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `d`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeDefeatDialogBJ takes player whichPlayer, boolean leftGame returns nothing
    local trigger t = CreateTrigger()
    local dialog  d = DialogCreate()
    local string formatString

    // Display "player was defeated" or "player has left the game" message
    if (leftGame) then
        set formatString = GetLocalizedString( "PLAYER_LEFT_GAME" )
    else
        set formatString = GetLocalizedString( "PLAYER_DEFEATED" )
    endif

    call DisplayTimedTextFromPlayer(whichPlayer, 0, 0, 60, formatString)

    call DialogSetMessage( d, GetLocalizedString( "GAMEOVER_DEFEAT_MSG" ) )

    // Only show the continue button if the game is not over and observers on death are allowed
    if (not bj_meleeGameOver and IsMapFlagSet(MAP_OBSERVERS_ON_DEATH)) then
        call DialogAddButton( d, GetLocalizedString( "GAMEOVER_CONTINUE_OBSERVING" ), GetLocalizedHotkey("GAMEOVER_CONTINUE_OBSERVING") )
    endif

    set t = CreateTrigger()
    call TriggerRegisterDialogButtonEvent( t, DialogAddQuitButton( d, true, GetLocalizedString( "GAMEOVER_QUIT_GAME" ), GetLocalizedHotkey("GAMEOVER_QUIT_GAME") ) )

    call DialogDisplay( whichPlayer, d, true )
    call StartSoundForPlayerBJ( whichPlayer, bj_defeatDialogSound )
endfunction

//===========================================================================

/**
@bug Leaks handle `t`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `d`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function GameOverDialogBJ takes player whichPlayer, boolean leftGame returns nothing
    local trigger t = CreateTrigger()
    local dialog  d = DialogCreate()
    local string  s

    // Display "player left the game" message
    call DisplayTimedTextFromPlayer(whichPlayer, 0, 0, 60, GetLocalizedString( "PLAYER_LEFT_GAME" ))

    if (GetIntegerGameState(GAME_STATE_DISCONNECTED) != 0) then
        set s = GetLocalizedString( "GAMEOVER_DISCONNECTED" )
    else
        set s = GetLocalizedString( "GAMEOVER_GAME_OVER" )
    endif

    call DialogSetMessage( d, s )

    set t = CreateTrigger()
    call TriggerRegisterDialogButtonEvent( t, DialogAddQuitButton( d, true, GetLocalizedString( "GAMEOVER_OK" ), GetLocalizedHotkey("GAMEOVER_OK") ) )

    call DialogDisplay( whichPlayer, d, true )
    call StartSoundForPlayerBJ( whichPlayer, bj_defeatDialogSound )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RemovePlayerPreserveUnitsBJ takes player whichPlayer, playergameresult gameResult, boolean leftGame returns nothing
    if AllowVictoryDefeat(gameResult) then

        call RemovePlayer(whichPlayer, gameResult)

        if( gameResult == PLAYER_GAME_RESULT_VICTORY ) then
            call MeleeVictoryDialogBJ( whichPlayer, leftGame )
            return
        elseif( gameResult == PLAYER_GAME_RESULT_DEFEAT ) then
            call MeleeDefeatDialogBJ( whichPlayer, leftGame )
        else
            call GameOverDialogBJ( whichPlayer, leftGame )
        endif

    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomVictoryOkBJ takes nothing returns nothing
    if bj_isSinglePlayer then
        call PauseGame( false )
        // Bump the difficulty back up to the default.
        call SetGameDifficulty(GetDefaultDifficulty())
    endif

    if (bj_changeLevelMapName == null) then
        call EndGame( bj_changeLevelShowScores )
    else
        call ChangeLevel( bj_changeLevelMapName, bj_changeLevelShowScores )
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomVictoryQuitBJ takes nothing returns nothing
    if bj_isSinglePlayer then
        call PauseGame( false )
        // Bump the difficulty back up to the default.
        call SetGameDifficulty(GetDefaultDifficulty())
    endif

    call EndGame( bj_changeLevelShowScores )
endfunction

//===========================================================================

/**
@bug Leaks handle `t`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `d`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function CustomVictoryDialogBJ takes player whichPlayer returns nothing
    local trigger t = CreateTrigger()
    local dialog  d = DialogCreate()

    call DialogSetMessage( d, GetLocalizedString( "GAMEOVER_VICTORY_MSG" ) )

    set t = CreateTrigger()
    call TriggerRegisterDialogButtonEvent( t, DialogAddButton( d, GetLocalizedString( "GAMEOVER_CONTINUE" ), GetLocalizedHotkey("GAMEOVER_CONTINUE") ) )
    call TriggerAddAction( t, function CustomVictoryOkBJ )

    set t = CreateTrigger()
    call TriggerRegisterDialogButtonEvent( t, DialogAddButton( d, GetLocalizedString( "GAMEOVER_QUIT_MISSION" ), GetLocalizedHotkey("GAMEOVER_QUIT_MISSION") ) )
    call TriggerAddAction( t, function CustomVictoryQuitBJ )

    if (GetLocalPlayer() == whichPlayer) then
        call EnableUserControl( true )
        if bj_isSinglePlayer then
            call PauseGame( true )
        endif
        call EnableUserUI(false)
    endif

    call DialogDisplay( whichPlayer, d, true )
    call VolumeGroupSetVolumeForPlayerBJ( whichPlayer, SOUND_VOLUMEGROUP_UI, 1.0 )
    call StartSoundForPlayerBJ( whichPlayer, bj_victoryDialogSound )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomVictorySkipBJ takes player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        if bj_isSinglePlayer then
            // Bump the difficulty back up to the default.
            call SetGameDifficulty(GetDefaultDifficulty())
        endif

        if (bj_changeLevelMapName == null) then
            call EndGame( bj_changeLevelShowScores )
        else
            call ChangeLevel( bj_changeLevelMapName, bj_changeLevelShowScores )
        endif
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomVictoryBJ takes player whichPlayer, boolean showDialog, boolean showScores returns nothing
    if AllowVictoryDefeat( PLAYER_GAME_RESULT_VICTORY ) then
        call RemovePlayer( whichPlayer, PLAYER_GAME_RESULT_VICTORY )

        if not bj_isSinglePlayer then
            call DisplayTimedTextFromPlayer(whichPlayer, 0, 0, 60, GetLocalizedString( "PLAYER_VICTORIOUS" ) )
        endif

        // UI only needs to be displayed to users.
        if (GetPlayerController(whichPlayer) == MAP_CONTROL_USER) then
            set bj_changeLevelShowScores = showScores
            if showDialog then
                call CustomVictoryDialogBJ( whichPlayer )
            else
                call CustomVictorySkipBJ( whichPlayer )
            endif
        endif
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomDefeatRestartBJ takes nothing returns nothing
    call PauseGame( false )
    call RestartGame( true )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomDefeatReduceDifficultyBJ takes nothing returns nothing
    local gamedifficulty diff = GetGameDifficulty()

    call PauseGame( false )

    // Knock the difficulty down, if possible.
    if (diff == MAP_DIFFICULTY_EASY) then
        // Sorry, but it doesn't get any easier than this.
    elseif (diff == MAP_DIFFICULTY_NORMAL) then
        call SetGameDifficulty(MAP_DIFFICULTY_EASY)
    elseif (diff == MAP_DIFFICULTY_HARD) then
        call SetGameDifficulty(MAP_DIFFICULTY_NORMAL)
    else
        // Unrecognized difficulty
    endif

    call RestartGame( true )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomDefeatLoadBJ takes nothing returns nothing
    call PauseGame( false )
    call DisplayLoadDialog()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomDefeatQuitBJ takes nothing returns nothing
    if bj_isSinglePlayer then
        call PauseGame( false )
    endif

    // Bump the difficulty back up to the default.
    call SetGameDifficulty(GetDefaultDifficulty())
    call EndGame( true )
endfunction

//===========================================================================

/**
@bug Leaks handle `t`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `d`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function CustomDefeatDialogBJ takes player whichPlayer, string message returns nothing
    local trigger t = CreateTrigger()
    local dialog  d = DialogCreate()

    call DialogSetMessage( d, message )

    if bj_isSinglePlayer then
        set t = CreateTrigger()
        call TriggerRegisterDialogButtonEvent( t, DialogAddButton( d, GetLocalizedString( "GAMEOVER_RESTART" ), GetLocalizedHotkey("GAMEOVER_RESTART") ) )
        call TriggerAddAction( t, function CustomDefeatRestartBJ )

        if (GetGameDifficulty() != MAP_DIFFICULTY_EASY) then
            set t = CreateTrigger()
            call TriggerRegisterDialogButtonEvent( t, DialogAddButton( d, GetLocalizedString( "GAMEOVER_REDUCE_DIFFICULTY" ), GetLocalizedHotkey("GAMEOVER_REDUCE_DIFFICULTY") ) )
            call TriggerAddAction( t, function CustomDefeatReduceDifficultyBJ )
        endif

        set t = CreateTrigger()
        call TriggerRegisterDialogButtonEvent( t, DialogAddButton( d, GetLocalizedString( "GAMEOVER_LOAD" ), GetLocalizedHotkey("GAMEOVER_LOAD") ) )
        call TriggerAddAction( t, function CustomDefeatLoadBJ )
    endif

    set t = CreateTrigger()
    call TriggerRegisterDialogButtonEvent( t, DialogAddButton( d, GetLocalizedString( "GAMEOVER_QUIT_MISSION" ), GetLocalizedHotkey("GAMEOVER_QUIT_MISSION") ) )
    call TriggerAddAction( t, function CustomDefeatQuitBJ )

    if (GetLocalPlayer() == whichPlayer) then
        call EnableUserControl( true )
        if bj_isSinglePlayer then
            call PauseGame( true )
        endif
        call EnableUserUI(false)
    endif

    call DialogDisplay( whichPlayer, d, true )
    call VolumeGroupSetVolumeForPlayerBJ( whichPlayer, SOUND_VOLUMEGROUP_UI, 1.0 )
    call StartSoundForPlayerBJ( whichPlayer, bj_defeatDialogSound )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CustomDefeatBJ takes player whichPlayer, string message returns nothing
    if AllowVictoryDefeat( PLAYER_GAME_RESULT_DEFEAT ) then
        call RemovePlayer( whichPlayer, PLAYER_GAME_RESULT_DEFEAT )

        if not bj_isSinglePlayer then
            call DisplayTimedTextFromPlayer(whichPlayer, 0, 0, 60, GetLocalizedString( "PLAYER_DEFEATED" ) )
        endif

        // UI only needs to be displayed to users.
        if (GetPlayerController(whichPlayer) == MAP_CONTROL_USER) then
            call CustomDefeatDialogBJ( whichPlayer, message )
        endif
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetNextLevelBJ takes string nextLevel returns nothing
    if (nextLevel == "") then
        set bj_changeLevelMapName = null
    else
        set bj_changeLevelMapName = nextLevel
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerOnScoreScreenBJ takes boolean flag, player whichPlayer returns nothing
    call SetPlayerOnScoreScreen(whichPlayer, flag)
endfunction



//***************************************************************************
//*
//*  Quest Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function CreateQuestBJ takes integer questType, string title, string description, string iconPath returns quest
    local boolean required   = (questType == bj_QUESTTYPE_REQ_DISCOVERED) or (questType == bj_QUESTTYPE_REQ_UNDISCOVERED)
    local boolean discovered = (questType == bj_QUESTTYPE_REQ_DISCOVERED) or (questType == bj_QUESTTYPE_OPT_DISCOVERED)

    set bj_lastCreatedQuest = CreateQuest()
    call QuestSetTitle(bj_lastCreatedQuest, title)
    call QuestSetDescription(bj_lastCreatedQuest, description)
    call QuestSetIconPath(bj_lastCreatedQuest, iconPath)
    call QuestSetRequired(bj_lastCreatedQuest, required)
    call QuestSetDiscovered(bj_lastCreatedQuest, discovered)
    call QuestSetCompleted(bj_lastCreatedQuest, false)
    return bj_lastCreatedQuest
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DestroyQuestBJ takes quest whichQuest returns nothing
    call DestroyQuest(whichQuest)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestSetEnabledBJ takes boolean enabled, quest whichQuest returns nothing
    call QuestSetEnabled(whichQuest, enabled)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestSetTitleBJ takes quest whichQuest, string title returns nothing
    call QuestSetTitle(whichQuest, title)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestSetDescriptionBJ takes quest whichQuest, string description returns nothing
    call QuestSetDescription(whichQuest, description)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestSetCompletedBJ takes quest whichQuest, boolean completed returns nothing
    call QuestSetCompleted(whichQuest, completed)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestSetFailedBJ takes quest whichQuest, boolean failed returns nothing
    call QuestSetFailed(whichQuest, failed)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestSetDiscoveredBJ takes quest whichQuest, boolean discovered returns nothing
    call QuestSetDiscovered(whichQuest, discovered)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedQuestBJ takes nothing returns quest
    return bj_lastCreatedQuest
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateQuestItemBJ takes quest whichQuest, string description returns questitem
    set bj_lastCreatedQuestItem = QuestCreateItem(whichQuest)
    call QuestItemSetDescription(bj_lastCreatedQuestItem, description)
    call QuestItemSetCompleted(bj_lastCreatedQuestItem, false)
    return bj_lastCreatedQuestItem
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestItemSetDescriptionBJ takes questitem whichQuestItem, string description returns nothing
    call QuestItemSetDescription(whichQuestItem, description)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestItemSetCompletedBJ takes questitem whichQuestItem, boolean completed returns nothing
    call QuestItemSetCompleted(whichQuestItem, completed)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedQuestItemBJ takes nothing returns questitem
    return bj_lastCreatedQuestItem
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateDefeatConditionBJ takes string description returns defeatcondition
    set bj_lastCreatedDefeatCondition = CreateDefeatCondition()
    call DefeatConditionSetDescription(bj_lastCreatedDefeatCondition, description)
    return bj_lastCreatedDefeatCondition
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DestroyDefeatConditionBJ takes defeatcondition whichCondition returns nothing
    call DestroyDefeatCondition(whichCondition)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DefeatConditionSetDescriptionBJ takes defeatcondition whichCondition, string description returns nothing
    call DefeatConditionSetDescription(whichCondition, description)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedDefeatConditionBJ takes nothing returns defeatcondition
    return bj_lastCreatedDefeatCondition
endfunction

//===========================================================================

/**
@patch 1.00
*/
function FlashQuestDialogButtonBJ takes nothing returns nothing
    call FlashQuestDialogButton()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function QuestMessageBJ takes force f, integer messageType, string message returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), f)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.

        if (messageType == bj_QUESTMESSAGE_DISCOVERED) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUEST, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUEST, message)
            call StartSound(bj_questDiscoveredSound)
            call FlashQuestDialogButton()

        elseif (messageType == bj_QUESTMESSAGE_UPDATED) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUESTUPDATE, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUESTUPDATE, message)
            call StartSound(bj_questUpdatedSound)
            call FlashQuestDialogButton()

        elseif (messageType == bj_QUESTMESSAGE_COMPLETED) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUESTDONE, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUESTDONE, message)
            call StartSound(bj_questCompletedSound)
            call FlashQuestDialogButton()

        elseif (messageType == bj_QUESTMESSAGE_FAILED) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUESTFAILED, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUESTFAILED, message)
            call StartSound(bj_questFailedSound)
            call FlashQuestDialogButton()

        elseif (messageType == bj_QUESTMESSAGE_REQUIREMENT) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_QUESTREQUIREMENT, message)

        elseif (messageType == bj_QUESTMESSAGE_MISSIONFAILED) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_MISSIONFAILED, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_MISSIONFAILED, message)
            call StartSound(bj_questFailedSound)

        elseif (messageType == bj_QUESTMESSAGE_HINT) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_HINT, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_HINT, message)
            call StartSound(bj_questHintSound)

        elseif (messageType == bj_QUESTMESSAGE_ALWAYSHINT) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_ALWAYSHINT, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_ALWAYSHINT, message)
            call StartSound(bj_questHintSound)

        elseif (messageType == bj_QUESTMESSAGE_SECRET) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_SECRET, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_SECRET, message)
            call StartSound(bj_questSecretSound)

        elseif (messageType == bj_QUESTMESSAGE_UNITACQUIRED) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_UNITACQUIRED, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_UNITACQUIRED, message)
            call StartSound(bj_questHintSound)

        elseif (messageType == bj_QUESTMESSAGE_UNITAVAILABLE) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_UNITAVAILABLE, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_UNITAVAILABLE, message)
            call StartSound(bj_questHintSound)

        elseif (messageType == bj_QUESTMESSAGE_ITEMACQUIRED) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_ITEMACQUIRED, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_ITEMACQUIRED, message)
            call StartSound(bj_questItemAcquiredSound)

        elseif (messageType == bj_QUESTMESSAGE_WARNING) then
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_WARNING, " ")
            call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_TEXT_DELAY_WARNING, message)
            call StartSound(bj_questWarningSound)

        else
            // Unrecognized message type - ignore the request.
        endif
    endif
endfunction



//***************************************************************************
//*
//*  Timer Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function StartTimerBJ takes timer t, boolean periodic, real timeout returns timer
    set bj_lastStartedTimer = t
    call TimerStart(t, timeout, periodic, null)
    return bj_lastStartedTimer
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateTimerBJ takes boolean periodic, real timeout returns timer
    set bj_lastStartedTimer = CreateTimer()
    call TimerStart(bj_lastStartedTimer, timeout, periodic, null)
    return bj_lastStartedTimer
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DestroyTimerBJ takes timer whichTimer returns nothing
    call DestroyTimer(whichTimer)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PauseTimerBJ takes boolean pause, timer whichTimer returns nothing
    if pause then
        call PauseTimer(whichTimer)
    else
        call ResumeTimer(whichTimer)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedTimerBJ takes nothing returns timer
    return bj_lastStartedTimer
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateTimerDialogBJ takes timer t, string title returns timerdialog
    set bj_lastCreatedTimerDialog = CreateTimerDialog(t)
    call TimerDialogSetTitle(bj_lastCreatedTimerDialog, title)
    call TimerDialogDisplay(bj_lastCreatedTimerDialog, true)
    return bj_lastCreatedTimerDialog
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DestroyTimerDialogBJ takes timerdialog td returns nothing
    call DestroyTimerDialog(td)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TimerDialogSetTitleBJ takes timerdialog td, string title returns nothing
    call TimerDialogSetTitle(td, title)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TimerDialogSetTitleColorBJ takes timerdialog td, real red, real green, real blue, real transparency returns nothing
    call TimerDialogSetTitleColor(td, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TimerDialogSetTimeColorBJ takes timerdialog td, real red, real green, real blue, real transparency returns nothing
    call TimerDialogSetTimeColor(td, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TimerDialogSetSpeedBJ takes timerdialog td, real speedMultFactor returns nothing
    call TimerDialogSetSpeed(td, speedMultFactor)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function TimerDialogDisplayForPlayerBJ takes boolean show, timerdialog td, player whichPlayer returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call TimerDialogDisplay(td, show)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function TimerDialogDisplayBJ takes boolean show, timerdialog td returns nothing
    call TimerDialogDisplay(td, show)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedTimerDialogBJ takes nothing returns timerdialog
    return bj_lastCreatedTimerDialog
endfunction



//***************************************************************************
//*
//*  Leaderboard Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardResizeBJ takes leaderboard lb returns nothing
    local integer size = LeaderboardGetItemCount(lb)

    if (LeaderboardGetLabelText(lb) == "") then
        set size = size - 1
    endif
    call LeaderboardSetSizeByItemCount(lb, size)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetPlayerItemValueBJ takes player whichPlayer, leaderboard lb, integer val returns nothing
    call LeaderboardSetItemValue(lb, LeaderboardGetPlayerIndex(lb, whichPlayer), val)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetPlayerItemLabelBJ takes player whichPlayer, leaderboard lb, string val returns nothing
    call LeaderboardSetItemLabel(lb, LeaderboardGetPlayerIndex(lb, whichPlayer), val)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetPlayerItemStyleBJ takes player whichPlayer, leaderboard lb, boolean showLabel, boolean showValue, boolean showIcon returns nothing
    call LeaderboardSetItemStyle(lb, LeaderboardGetPlayerIndex(lb, whichPlayer), showLabel, showValue, showIcon)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetPlayerItemLabelColorBJ takes player whichPlayer, leaderboard lb, real red, real green, real blue, real transparency returns nothing
    call LeaderboardSetItemLabelColor(lb, LeaderboardGetPlayerIndex(lb, whichPlayer), PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetPlayerItemValueColorBJ takes player whichPlayer, leaderboard lb, real red, real green, real blue, real transparency returns nothing
    call LeaderboardSetItemValueColor(lb, LeaderboardGetPlayerIndex(lb, whichPlayer), PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetLabelColorBJ takes leaderboard lb, real red, real green, real blue, real transparency returns nothing
    call LeaderboardSetLabelColor(lb, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetValueColorBJ takes leaderboard lb, real red, real green, real blue, real transparency returns nothing
    call LeaderboardSetValueColor(lb, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetLabelBJ takes leaderboard lb, string label returns nothing
    call LeaderboardSetLabel(lb, label)
    call LeaderboardResizeBJ(lb)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSetStyleBJ takes leaderboard lb, boolean showLabel, boolean showNames, boolean showValues, boolean showIcons returns nothing
    call LeaderboardSetStyle(lb, showLabel, showNames, showValues, showIcons)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardGetItemCountBJ takes leaderboard lb returns integer
    return LeaderboardGetItemCount(lb)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardHasPlayerItemBJ takes leaderboard lb, player whichPlayer returns boolean
    return LeaderboardHasPlayerItem(lb, whichPlayer)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ForceSetLeaderboardBJ takes leaderboard lb, force toForce returns nothing
    local integer index
    local player  indexPlayer

    set index = 0
    loop
        set indexPlayer = Player(index)
        if IsPlayerInForce(indexPlayer, toForce) then
            call PlayerSetLeaderboard(indexPlayer, lb)
        endif
        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CreateLeaderboardBJ takes force toForce, string label returns leaderboard
    set bj_lastCreatedLeaderboard = CreateLeaderboard()
    call LeaderboardSetLabel(bj_lastCreatedLeaderboard, label)
    call ForceSetLeaderboardBJ(bj_lastCreatedLeaderboard, toForce)
    call LeaderboardDisplay(bj_lastCreatedLeaderboard, true)
    return bj_lastCreatedLeaderboard
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DestroyLeaderboardBJ takes leaderboard lb returns nothing
    call DestroyLeaderboard(lb)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardDisplayBJ takes boolean show, leaderboard lb returns nothing
    call LeaderboardDisplay(lb, show)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardAddItemBJ takes player whichPlayer, leaderboard lb, string label, integer value returns nothing
    if (LeaderboardHasPlayerItem(lb, whichPlayer)) then
        call LeaderboardRemovePlayerItem(lb, whichPlayer)
    endif
    call LeaderboardAddItem(lb, label, value, whichPlayer)
    call LeaderboardResizeBJ(lb)
    //call LeaderboardSetSizeByItemCount(lb, LeaderboardGetItemCount(lb))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardRemovePlayerItemBJ takes player whichPlayer, leaderboard lb returns nothing
    call LeaderboardRemovePlayerItem(lb, whichPlayer)
    call LeaderboardResizeBJ(lb)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSortItemsBJ takes leaderboard lb, integer sortType, boolean ascending returns nothing
    if (sortType == bj_SORTTYPE_SORTBYVALUE) then
        call LeaderboardSortItemsByValue(lb, ascending)
    elseif (sortType == bj_SORTTYPE_SORTBYPLAYER) then
        call LeaderboardSortItemsByPlayer(lb, ascending)
    elseif (sortType == bj_SORTTYPE_SORTBYLABEL) then
        call LeaderboardSortItemsByLabel(lb, ascending)
    else
        // Unrecognized sort type - ignore the request.
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSortItemsByPlayerBJ takes leaderboard lb, boolean ascending returns nothing
    call LeaderboardSortItemsByPlayer(lb, ascending)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardSortItemsByLabelBJ takes leaderboard lb, boolean ascending returns nothing
    call LeaderboardSortItemsByLabel(lb, ascending)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LeaderboardGetPlayerIndexBJ takes player whichPlayer, leaderboard lb returns integer
    return LeaderboardGetPlayerIndex(lb, whichPlayer) + 1
endfunction

//===========================================================================
// Returns the player who is occupying a specified position in a leaderboard.
// The position parameter is expected in the range of 1..16.
//

/**
@patch 1.07
*/
function LeaderboardGetIndexedPlayerBJ takes integer position, leaderboard lb returns player
    local integer index
    local player  indexPlayer

    set index = 0
    loop
        set indexPlayer = Player(index)
        if (LeaderboardGetPlayerIndex(lb, indexPlayer) == position - 1) then
            return indexPlayer
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop

    return Player(PLAYER_NEUTRAL_PASSIVE)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PlayerGetLeaderboardBJ takes player whichPlayer returns leaderboard
    return PlayerGetLeaderboard(whichPlayer)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedLeaderboard takes nothing returns leaderboard
    return bj_lastCreatedLeaderboard
endfunction

//***************************************************************************
//*
//*  Multiboard Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.13
*/
function CreateMultiboardBJ takes integer cols, integer rows, string title returns multiboard
    set bj_lastCreatedMultiboard = CreateMultiboard()
    call MultiboardSetRowCount(bj_lastCreatedMultiboard, rows)
    call MultiboardSetColumnCount(bj_lastCreatedMultiboard, cols)
    call MultiboardSetTitleText(bj_lastCreatedMultiboard, title)
    call MultiboardDisplay(bj_lastCreatedMultiboard, true)
    return bj_lastCreatedMultiboard
endfunction

//===========================================================================

/**
@patch 1.13
*/
function DestroyMultiboardBJ takes multiboard mb returns nothing
    call DestroyMultiboard(mb)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function GetLastCreatedMultiboard takes nothing returns multiboard
    return bj_lastCreatedMultiboard
endfunction

//===========================================================================

/**
@patch 1.13
*/
function MultiboardDisplayBJ takes boolean show, multiboard mb returns nothing
    call MultiboardDisplay(mb, show)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function MultiboardMinimizeBJ takes boolean minimize, multiboard mb returns nothing
    call MultiboardMinimize(mb, minimize)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function MultiboardSetTitleTextColorBJ takes multiboard mb, real red, real green, real blue, real transparency returns nothing
    call MultiboardSetTitleTextColor(mb, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function MultiboardAllowDisplayBJ takes boolean flag returns nothing
    call MultiboardSuppressDisplay(not flag)
endfunction

//===========================================================================

/**
@bug Leaks handle `mbitem`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.13
*/
function MultiboardSetItemStyleBJ takes multiboard mb, integer col, integer row, boolean showValue, boolean showIcon returns nothing
    local integer curRow = 0
    local integer curCol = 0
    local integer numRows = MultiboardGetRowCount(mb)
    local integer numCols = MultiboardGetColumnCount(mb)
    local multiboarditem mbitem = null

    // Loop over rows, using 1-based index
    loop
        set curRow = curRow + 1
        exitwhen curRow > numRows

        // Apply setting to the requested row, or all rows (if row is 0)
        if (row == 0 or row == curRow) then
            // Loop over columns, using 1-based index
            set curCol = 0
            loop
                set curCol = curCol + 1
                exitwhen curCol > numCols

                // Apply setting to the requested column, or all columns (if col is 0)
                if (col == 0 or col == curCol) then
                    set mbitem = MultiboardGetItem(mb, curRow - 1, curCol - 1)
                    call MultiboardSetItemStyle(mbitem, showValue, showIcon)
                    call MultiboardReleaseItem(mbitem)
                endif
            endloop
        endif
    endloop
endfunction

//===========================================================================

/**
@bug Leaks handle `mbitem`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.13
*/
function MultiboardSetItemValueBJ takes multiboard mb, integer col, integer row, string val returns nothing
    local integer curRow = 0
    local integer curCol = 0
    local integer numRows = MultiboardGetRowCount(mb)
    local integer numCols = MultiboardGetColumnCount(mb)
    local multiboarditem mbitem = null

    // Loop over rows, using 1-based index
    loop
        set curRow = curRow + 1
        exitwhen curRow > numRows

        // Apply setting to the requested row, or all rows (if row is 0)
        if (row == 0 or row == curRow) then
            // Loop over columns, using 1-based index
            set curCol = 0
            loop
                set curCol = curCol + 1
                exitwhen curCol > numCols

                // Apply setting to the requested column, or all columns (if col is 0)
                if (col == 0 or col == curCol) then
                    set mbitem = MultiboardGetItem(mb, curRow - 1, curCol - 1)
                    call MultiboardSetItemValue(mbitem, val)
                    call MultiboardReleaseItem(mbitem)
                endif
            endloop
        endif
    endloop
endfunction

//===========================================================================

/**
@bug Leaks handle `mbitem`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.13
*/
function MultiboardSetItemColorBJ takes multiboard mb, integer col, integer row, real red, real green, real blue, real transparency returns nothing
    local integer curRow = 0
    local integer curCol = 0
    local integer numRows = MultiboardGetRowCount(mb)
    local integer numCols = MultiboardGetColumnCount(mb)
    local multiboarditem mbitem = null

    // Loop over rows, using 1-based index
    loop
        set curRow = curRow + 1
        exitwhen curRow > numRows

        // Apply setting to the requested row, or all rows (if row is 0)
        if (row == 0 or row == curRow) then
            // Loop over columns, using 1-based index
            set curCol = 0
            loop
                set curCol = curCol + 1
                exitwhen curCol > numCols

                // Apply setting to the requested column, or all columns (if col is 0)
                if (col == 0 or col == curCol) then
                    set mbitem = MultiboardGetItem(mb, curRow - 1, curCol - 1)
                    call MultiboardSetItemValueColor(mbitem, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
                    call MultiboardReleaseItem(mbitem)
                endif
            endloop
        endif
    endloop
endfunction

//===========================================================================

/**
@bug Leaks handle `mbitem`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.13
*/
function MultiboardSetItemWidthBJ takes multiboard mb, integer col, integer row, real width returns nothing
    local integer curRow = 0
    local integer curCol = 0
    local integer numRows = MultiboardGetRowCount(mb)
    local integer numCols = MultiboardGetColumnCount(mb)
    local multiboarditem mbitem = null

    // Loop over rows, using 1-based index
    loop
        set curRow = curRow + 1
        exitwhen curRow > numRows

        // Apply setting to the requested row, or all rows (if row is 0)
        if (row == 0 or row == curRow) then
            // Loop over columns, using 1-based index
            set curCol = 0
            loop
                set curCol = curCol + 1
                exitwhen curCol > numCols

                // Apply setting to the requested column, or all columns (if col is 0)
                if (col == 0 or col == curCol) then
                    set mbitem = MultiboardGetItem(mb, curRow - 1, curCol - 1)
                    call MultiboardSetItemWidth(mbitem, width/100.0)
                    call MultiboardReleaseItem(mbitem)
                endif
            endloop
        endif
    endloop
endfunction

//===========================================================================

/**
@bug Leaks handle `mbitem`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.13
*/
function MultiboardSetItemIconBJ takes multiboard mb, integer col, integer row, string iconFileName returns nothing
    local integer curRow = 0
    local integer curCol = 0
    local integer numRows = MultiboardGetRowCount(mb)
    local integer numCols = MultiboardGetColumnCount(mb)
    local multiboarditem mbitem = null

    // Loop over rows, using 1-based index
    loop
        set curRow = curRow + 1
        exitwhen curRow > numRows

        // Apply setting to the requested row, or all rows (if row is 0)
        if (row == 0 or row == curRow) then
            // Loop over columns, using 1-based index
            set curCol = 0
            loop
                set curCol = curCol + 1
                exitwhen curCol > numCols

                // Apply setting to the requested column, or all columns (if col is 0)
                if (col == 0 or col == curCol) then
                    set mbitem = MultiboardGetItem(mb, curRow - 1, curCol - 1)
                    call MultiboardSetItemIcon(mbitem, iconFileName)
                    call MultiboardReleaseItem(mbitem)
                endif
            endloop
        endif
    endloop
endfunction



//***************************************************************************
//*
//*  Text Tag Utility Functions
//*
//***************************************************************************

//===========================================================================
// Scale the font size linearly such that size 10 equates to height 0.023.
// Screen-relative font heights are harder to grasp and than font sizes.
//

/**
@patch 1.07
*/
function TextTagSize2Height takes real size returns real
    return size * 0.023 / 10
endfunction

//===========================================================================
// Scale the speed linearly such that speed 128 equates to 0.071.
// Screen-relative speeds are hard to grasp.
//

/**
@patch 1.07
*/
function TextTagSpeed2Velocity takes real speed returns real
    return speed * 0.071 / 128
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetTextTagColorBJ takes texttag tt, real red, real green, real blue, real transparency returns nothing
    call SetTextTagColor(tt, PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-transparency))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetTextTagVelocityBJ takes texttag tt, real speed, real angle returns nothing
    local real vel = TextTagSpeed2Velocity(speed)
    local real xvel = vel * Cos(angle * bj_DEGTORAD)
    local real yvel = vel * Sin(angle * bj_DEGTORAD)

    call SetTextTagVelocity(tt, xvel, yvel)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagTextBJ takes texttag tt, string s, real size returns nothing
    local real textHeight = TextTagSize2Height(size)

    call SetTextTagText(tt, s, textHeight)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagPosBJ takes texttag tt, location loc, real zOffset returns nothing
    call SetTextTagPos(tt, GetLocationX(loc), GetLocationY(loc), zOffset)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagPosUnitBJ takes texttag tt, unit whichUnit, real zOffset returns nothing
    call SetTextTagPosUnit(tt, whichUnit, zOffset)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagSuspendedBJ takes texttag tt, boolean flag returns nothing
    call SetTextTagSuspended(tt, flag)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagPermanentBJ takes texttag tt, boolean flag returns nothing
    call SetTextTagPermanent(tt, flag)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagAgeBJ takes texttag tt, real age returns nothing
    call SetTextTagAge(tt, age)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagLifespanBJ takes texttag tt, real lifespan returns nothing
    call SetTextTagLifespan(tt, lifespan)
endfunction

//===========================================================================

/**
@patch 1.18a
*/
function SetTextTagFadepointBJ takes texttag tt, real fadepoint returns nothing
    call SetTextTagFadepoint(tt, fadepoint)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function CreateTextTagLocBJ takes string s, location loc, real zOffset, real size, real red, real green, real blue, real transparency returns texttag
    set bj_lastCreatedTextTag = CreateTextTag()
    call SetTextTagTextBJ(bj_lastCreatedTextTag, s, size)
    call SetTextTagPosBJ(bj_lastCreatedTextTag, loc, zOffset)
    call SetTextTagColorBJ(bj_lastCreatedTextTag, red, green, blue, transparency)

    return bj_lastCreatedTextTag
endfunction

//===========================================================================

/**
@patch 1.07
*/
function CreateTextTagUnitBJ takes string s, unit whichUnit, real zOffset, real size, real red, real green, real blue, real transparency returns texttag
    set bj_lastCreatedTextTag = CreateTextTag()
    call SetTextTagTextBJ(bj_lastCreatedTextTag, s, size)
    call SetTextTagPosUnitBJ(bj_lastCreatedTextTag, whichUnit, zOffset)
    call SetTextTagColorBJ(bj_lastCreatedTextTag, red, green, blue, transparency)

    return bj_lastCreatedTextTag
endfunction

//===========================================================================

/**
@patch 1.07
*/
function DestroyTextTagBJ takes texttag tt returns nothing
    call DestroyTextTag(tt)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ShowTextTagForceBJ takes boolean show, texttag tt, force whichForce returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), whichForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call SetTextTagVisibility(tt, show)
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function GetLastCreatedTextTag takes nothing returns texttag
    return bj_lastCreatedTextTag
endfunction



//***************************************************************************
//*
//*  Cinematic Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function PauseGameOn takes nothing returns nothing
    call PauseGame(true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PauseGameOff takes nothing returns nothing
    call PauseGame(false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUserControlForceOn takes force whichForce returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), whichForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call EnableUserControl(true)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetUserControlForceOff takes force whichForce returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), whichForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call EnableUserControl(false)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ShowInterfaceForceOn takes force whichForce, real fadeDuration returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), whichForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ShowInterface(true, fadeDuration)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ShowInterfaceForceOff takes force whichForce, real fadeDuration returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), whichForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call ShowInterface(false, fadeDuration)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PingMinimapForForce takes force whichForce, real x, real y, real duration returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), whichForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call PingMinimap(x, y, duration)
        //call StartSound(bj_pingMinimapSound)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PingMinimapLocForForce takes force whichForce, location loc, real duration returns nothing
    call PingMinimapForForce(whichForce, GetLocationX(loc), GetLocationY(loc), duration)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PingMinimapForPlayer takes player whichPlayer, real x, real y, real duration returns nothing
    if (GetLocalPlayer() == whichPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call PingMinimap(x, y, duration)
        //call StartSound(bj_pingMinimapSound)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function PingMinimapLocForPlayer takes player whichPlayer, location loc, real duration returns nothing
    call PingMinimapForPlayer(whichPlayer, GetLocationX(loc), GetLocationY(loc), duration)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function PingMinimapForForceEx takes force whichForce, real x, real y, real duration, integer style, real red, real green, real blue returns nothing
    local integer red255   = PercentTo255(red)
    local integer green255 = PercentTo255(green)
    local integer blue255  = PercentTo255(blue)

    if (IsPlayerInForce(GetLocalPlayer(), whichForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.

        // Prevent 100% red simple and flashy pings, as they become "attack" pings.
        if (red255 == 255) and (green255 == 0) and (blue255 == 0) then
            set red255 = 254
        endif

        if (style == bj_MINIMAPPINGSTYLE_SIMPLE) then
            call PingMinimapEx(x, y, duration, red255, green255, blue255, false)
        elseif (style == bj_MINIMAPPINGSTYLE_FLASHY) then
            call PingMinimapEx(x, y, duration, red255, green255, blue255, true)
        elseif (style == bj_MINIMAPPINGSTYLE_ATTACK) then
            call PingMinimapEx(x, y, duration, 255, 0, 0, false)
        else
            // Unrecognized ping style - ignore the request.
        endif
        
        //call StartSound(bj_pingMinimapSound)
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function PingMinimapLocForForceEx takes force whichForce, location loc, real duration, integer style, real red, real green, real blue returns nothing
    call PingMinimapForForceEx(whichForce, GetLocationX(loc), GetLocationY(loc), duration, style, red, green, blue)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function EnableWorldFogBoundaryBJ takes boolean enable, force f returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), f)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call EnableWorldFogBoundary(enable)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function EnableOcclusionBJ takes boolean enable, force f returns nothing
    if (IsPlayerInForce(GetLocalPlayer(), f)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.
        call EnableOcclusion(enable)
    endif
endfunction



//***************************************************************************
//*
//*  Cinematic Transmission Utility Functions
//*
//***************************************************************************

//===========================================================================
// If cancelled, stop the sound and end the cinematic scene.
//

/**
@patch 1.00
*/
function CancelCineSceneBJ takes nothing returns nothing
    call StopSoundBJ(bj_cineSceneLastSound, true)
    call EndCinematicScene()
endfunction

//===========================================================================
// Init a trigger to listen for END_CINEMATIC events and respond to them if
// a cinematic scene is in progress.  For performance reasons, this should
// only be called once a cinematic scene has been started, so that maps
// lacking such scenes do not bother to register for these events.
//

/**
@patch 1.00
*/
function TryInitCinematicBehaviorBJ takes nothing returns nothing
    local integer index

    if (bj_cineSceneBeingSkipped == null) then
        set bj_cineSceneBeingSkipped = CreateTrigger()
        set index = 0
        loop
            call TriggerRegisterPlayerEvent(bj_cineSceneBeingSkipped, Player(index), EVENT_PLAYER_END_CINEMATIC)
            set index = index + 1
            exitwhen index == bj_MAX_PLAYERS
        endloop
        call TriggerAddAction(bj_cineSceneBeingSkipped, function CancelCineSceneBJ)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCinematicSceneBJ takes sound soundHandle, integer portraitUnitId, playercolor color, string speakerTitle, string text, real sceneDuration, real voiceoverDuration returns nothing
    set bj_cineSceneLastSound = soundHandle
    call SetCinematicScene(portraitUnitId, color, speakerTitle, text, sceneDuration, voiceoverDuration)
    call PlaySoundBJ(soundHandle)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetTransmissionDuration takes sound soundHandle, integer timeType, real timeVal returns real
    local real duration

    if (timeType == bj_TIMETYPE_ADD) then
        set duration = GetSoundDurationBJ(soundHandle) + timeVal
    elseif (timeType == bj_TIMETYPE_SET) then
        set duration = timeVal
    elseif (timeType == bj_TIMETYPE_SUB) then
        set duration = GetSoundDurationBJ(soundHandle) - timeVal
    else
        // Unrecognized timeType - ignore timeVal.
        set duration = GetSoundDurationBJ(soundHandle)
    endif

    // Make sure we have a non-negative duration.
    if (duration < 0) then
        set duration = 0
    endif
    return duration
endfunction

//===========================================================================

/**
@patch 1.00
*/
function WaitTransmissionDuration takes sound soundHandle, integer timeType, real timeVal returns nothing
    if (timeType == bj_TIMETYPE_SET) then
        // If we have a static duration wait, just perform the wait.
        call TriggerSleepAction(timeVal)

    elseif (soundHandle == null) then
        // If the sound does not exist, perform a default length wait.
        call TriggerSleepAction(bj_NOTHING_SOUND_DURATION)

    elseif (timeType == bj_TIMETYPE_SUB) then
        // If the transmission is cutting off the sound, wait for the sound
        // to be mostly finished.
        call WaitForSoundBJ(soundHandle, timeVal)

    elseif (timeType == bj_TIMETYPE_ADD) then
        // If the transmission is extending beyond the sound's length, wait
        // for it to finish, and then wait the additional time.
        call WaitForSoundBJ(soundHandle, 0)
        call TriggerSleepAction(timeVal)

    else
        // Unrecognized timeType - ignore.
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function DoTransmissionBasicsXYBJ takes integer unitId, playercolor color, real x, real y, sound soundHandle, string unitName, string message, real duration returns nothing
    call SetCinematicSceneBJ(soundHandle, unitId, color, unitName, message, duration + bj_TRANSMISSION_PORT_HANGTIME, duration)

    if (unitId != 0) then
        call PingMinimap(x, y, bj_TRANSMISSION_PING_TIME)
        //call SetCameraQuickPosition(x, y)
    endif
endfunction

//===========================================================================
// Display a text message to a Player Group with an accompanying sound,
// portrait, speech indicator, and all that good stuff.
//   - Query duration of sound
//   - Play sound
//   - Display text message for duration
//   - Display animating portrait for duration
//   - Display a speech indicator for the unit
//   - Ping the minimap
//

/**
@patch 1.00
*/
function TransmissionFromUnitWithNameBJ takes force toForce, unit whichUnit, string unitName, sound soundHandle, string message, integer timeType, real timeVal, boolean wait returns nothing
    call TryInitCinematicBehaviorBJ()

    call AttachSoundToUnit(soundHandle, whichUnit)

    // Ensure that the time value is non-negative.
    set timeVal = RMaxBJ(timeVal, 0)

    set bj_lastTransmissionDuration = GetTransmissionDuration(soundHandle, timeType, timeVal)
    set bj_lastPlayedSound = soundHandle

    if (IsPlayerInForce(GetLocalPlayer(), toForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.

        if (whichUnit == null) then
            // If the unit reference is invalid, send the transmission from the center of the map with no portrait.
            call DoTransmissionBasicsXYBJ(0, PLAYER_COLOR_RED, 0, 0, soundHandle, unitName, message, bj_lastTransmissionDuration)
        else
            call DoTransmissionBasicsXYBJ(GetUnitTypeId(whichUnit), GetPlayerColor(GetOwningPlayer(whichUnit)), GetUnitX(whichUnit), GetUnitY(whichUnit), soundHandle, unitName, message, bj_lastTransmissionDuration)
            if (not IsUnitHidden(whichUnit)) then
                call UnitAddIndicator(whichUnit, bj_TRANSMISSION_IND_RED, bj_TRANSMISSION_IND_BLUE, bj_TRANSMISSION_IND_GREEN, bj_TRANSMISSION_IND_ALPHA)
            endif
        endif
    endif

    if wait and (bj_lastTransmissionDuration > 0) then
        // call TriggerSleepAction(bj_lastTransmissionDuration)
        call WaitTransmissionDuration(soundHandle, timeType, timeVal)
    endif

endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function PlayDialogueFromSpeakerEx takes force toForce, unit speaker, integer speakerType, sound soundHandle, integer timeType, real timeVal, boolean wait returns boolean
    //Make sure that the runtime unit type and the parameter are the same,
    //otherwise the offline animations will not match and will fail
    if GetUnitTypeId(speaker) != speakerType then
        debug call BJDebugMsg(("Attempted to play FacialAnimation with the wrong speaker UnitType - Param: " + I2S(speakerType) + " Runtime: " +  I2S(GetUnitTypeId(speaker))))
        //return false
    endif

    call TryInitCinematicBehaviorBJ()

    call AttachSoundToUnit(soundHandle, speaker)

    // Ensure that the time value is non-negative.
    set timeVal = RMaxBJ(timeVal, 0)

    set bj_lastTransmissionDuration = GetTransmissionDuration(soundHandle, timeType, timeVal)
    set bj_lastPlayedSound = soundHandle

    if (IsPlayerInForce(GetLocalPlayer(), toForce)) then
        call SetCinematicSceneBJ(soundHandle, speakerType, GetPlayerColor(GetOwningPlayer(speaker)), GetLocalizedString(GetDialogueSpeakerNameKey(soundHandle)), GetLocalizedString(GetDialogueTextKey(soundHandle)), bj_lastTransmissionDuration + bj_TRANSMISSION_PORT_HANGTIME, bj_lastTransmissionDuration)
    endif

    if wait and (bj_lastTransmissionDuration > 0) then
        // call TriggerSleepAction(bj_lastTransmissionDuration)
        call WaitTransmissionDuration(soundHandle, timeType, timeVal)
    endif

    return true
endfunction

//===========================================================================

/**
@patch 1.32.0.13369
*/
function PlayDialogueFromSpeakerTypeEx takes force toForce, player fromPlayer, integer speakerType, location loc, sound soundHandle, integer timeType, real timeVal, boolean wait returns boolean
    call TryInitCinematicBehaviorBJ()

    // Ensure that the time value is non-negative.
    set timeVal = RMaxBJ(timeVal, 0)

    set bj_lastTransmissionDuration = GetTransmissionDuration(soundHandle, timeType, timeVal)
    set bj_lastPlayedSound = soundHandle

    if (IsPlayerInForce(GetLocalPlayer(), toForce)) then
        call SetCinematicSceneBJ(soundHandle, speakerType, GetPlayerColor(fromPlayer), GetLocalizedString(GetDialogueSpeakerNameKey(soundHandle)), GetLocalizedString(GetDialogueTextKey(soundHandle)), bj_lastTransmissionDuration + bj_TRANSMISSION_PORT_HANGTIME, bj_lastTransmissionDuration)
        if(speakerType != 0) then
            call PingMinimap(GetLocationX(loc), GetLocationY(loc), bj_TRANSMISSION_PING_TIME)
        endif
    endif

    if wait and (bj_lastTransmissionDuration > 0) then
        // call TriggerSleepAction(bj_lastTransmissionDuration)
        call WaitTransmissionDuration(soundHandle, timeType, timeVal)
    endif

    return true
endfunction

//===========================================================================
// This operates like TransmissionFromUnitWithNameBJ, but for a unit type
// rather than a unit instance.  As such, no speech indicator is employed.
//

/**
@patch 1.00
*/
function TransmissionFromUnitTypeWithNameBJ takes force toForce, player fromPlayer, integer unitId, string unitName, location loc, sound soundHandle, string message, integer timeType, real timeVal, boolean wait returns nothing
    call TryInitCinematicBehaviorBJ()

    // Ensure that the time value is non-negative.
    set timeVal = RMaxBJ(timeVal, 0)

    set bj_lastTransmissionDuration = GetTransmissionDuration(soundHandle, timeType, timeVal)
    set bj_lastPlayedSound = soundHandle

    if (IsPlayerInForce(GetLocalPlayer(), toForce)) then
        // Use only local code (no net traffic) within this block to avoid desyncs.

        call DoTransmissionBasicsXYBJ(unitId, GetPlayerColor(fromPlayer), GetLocationX(loc), GetLocationY(loc), soundHandle, unitName, message, bj_lastTransmissionDuration)
    endif

    if wait and (bj_lastTransmissionDuration > 0) then
        // call TriggerSleepAction(bj_lastTransmissionDuration)
        call WaitTransmissionDuration(soundHandle, timeType, timeVal)
    endif

endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastTransmissionDurationBJ takes nothing returns real
    return bj_lastTransmissionDuration
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ForceCinematicSubtitlesBJ takes boolean flag returns nothing
    call ForceCinematicSubtitles(flag)
endfunction


//***************************************************************************
//*
//*  Cinematic Mode Utility Functions
//*
//***************************************************************************

//===========================================================================
// Makes many common UI settings changes at once, for use when beginning and
// ending cinematic sequences.  Note that some affects apply to all players,
// such as game speed.  This is unavoidable.
//   - Clear the screen of text messages
//   - Hide interface UI (letterbox mode)
//   - Hide game messages (ally under attack, etc.)
//   - Disable user control
//   - Disable occlusion
//   - Set game speed (for all players)
//   - Lock game speed (for all players)
//   - Disable black mask (for all players)
//   - Disable fog of war (for all players)
//   - Disable world boundary fog (for all players)
//   - Dim non-speech sound channels
//   - End any outstanding music themes
//   - Fix the random seed to a set value
//   - Reset the camera smoothing factor
//

/**
@patch 1.07
*/
function CinematicModeExBJ takes boolean cineMode, force forForce, real interfaceFadeTime returns nothing
    // If the game hasn't started yet, perform interface fades immediately
    if (not bj_gameStarted) then
        set interfaceFadeTime = 0
    endif

    if (cineMode) then
        // Save the UI state so that we can restore it later.
        if (not bj_cineModeAlreadyIn) then
            call SetCinematicAudio(true)
            set bj_cineModeAlreadyIn = true
            set bj_cineModePriorSpeed = GetGameSpeed()
            set bj_cineModePriorFogSetting = IsFogEnabled()
            set bj_cineModePriorMaskSetting = IsFogMaskEnabled()
            set bj_cineModePriorDawnDusk = IsDawnDuskEnabled()
            set bj_cineModeSavedSeed = GetRandomInt(0, 1000000)
        endif

        // Perform local changes
        if (IsPlayerInForce(GetLocalPlayer(), forForce)) then
            // Use only local code (no net traffic) within this block to avoid desyncs.
            call ClearTextMessages()
            call ShowInterface(false, interfaceFadeTime)
            call EnableUserControl(false)
            call EnableOcclusion(false)
            call SetCineModeVolumeGroupsBJ()
        endif

        // Perform global changes
        call SetGameSpeed(bj_CINEMODE_GAMESPEED)
        call SetMapFlag(MAP_LOCK_SPEED, true)
        call FogMaskEnable(false)
        call FogEnable(false)
        call EnableWorldFogBoundary(false)
        call EnableDawnDusk(false)

        // Use a fixed random seed, so that cinematics play consistently.
        call SetRandomSeed(0)
    else
        set bj_cineModeAlreadyIn = false
        call SetCinematicAudio(false)

        // Perform local changes
        if (IsPlayerInForce(GetLocalPlayer(), forForce)) then
            // Use only local code (no net traffic) within this block to avoid desyncs.
            call ShowInterface(true, interfaceFadeTime)
            call EnableUserControl(true)
            call EnableOcclusion(true)
            call VolumeGroupReset()
            call EndThematicMusic()
            call CameraResetSmoothingFactorBJ()
        endif

        // Perform global changes
        call SetMapFlag(MAP_LOCK_SPEED, false)
        call SetGameSpeed(bj_cineModePriorSpeed)
        call FogMaskEnable(bj_cineModePriorMaskSetting)
        call FogEnable(bj_cineModePriorFogSetting)
        call EnableWorldFogBoundary(true)
        call EnableDawnDusk(bj_cineModePriorDawnDusk)
        call SetRandomSeed(bj_cineModeSavedSeed)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CinematicModeBJ takes boolean cineMode, force forForce returns nothing
    call CinematicModeExBJ(cineMode, forForce, bj_CINEMODE_INTERFACEFADE)
endfunction



//***************************************************************************
//*
//*  Cinematic Filter Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function DisplayCineFilterBJ takes boolean flag returns nothing
    call DisplayCineFilter(flag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CinematicFadeCommonBJ takes real red, real green, real blue, real duration, string tex, real startTrans, real endTrans returns nothing
    if (duration == 0) then
        // If the fade is instant, use the same starting and ending values,
        // so that we effectively do a set rather than a fade.
        set startTrans = endTrans
    endif
    call EnableUserUI(false)
    call SetCineFilterTexture(tex)
    call SetCineFilterBlendMode(BLEND_MODE_BLEND)
    call SetCineFilterTexMapFlags(TEXMAP_FLAG_NONE)
    call SetCineFilterStartUV(0, 0, 1, 1)
    call SetCineFilterEndUV(0, 0, 1, 1)
    call SetCineFilterStartColor(PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-startTrans))
    call SetCineFilterEndColor(PercentTo255(red), PercentTo255(green), PercentTo255(blue), PercentTo255(100.0-endTrans))
    call SetCineFilterDuration(duration)
    call DisplayCineFilter(true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function FinishCinematicFadeBJ takes nothing returns nothing
    call DestroyTimer(bj_cineFadeFinishTimer)
    set bj_cineFadeFinishTimer = null
    call DisplayCineFilter(false)
    call EnableUserUI(true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function FinishCinematicFadeAfterBJ takes real duration returns nothing
    // Create a timer to end the cinematic fade.
    set bj_cineFadeFinishTimer = CreateTimer()
    call TimerStart(bj_cineFadeFinishTimer, duration, false, function FinishCinematicFadeBJ)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ContinueCinematicFadeBJ takes nothing returns nothing
    call DestroyTimer(bj_cineFadeContinueTimer)
    set bj_cineFadeContinueTimer = null
    call CinematicFadeCommonBJ(bj_cineFadeContinueRed, bj_cineFadeContinueGreen, bj_cineFadeContinueBlue, bj_cineFadeContinueDuration, bj_cineFadeContinueTex, bj_cineFadeContinueTrans, 100)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ContinueCinematicFadeAfterBJ takes real duration, real red, real green, real blue, real trans, string tex returns nothing
    set bj_cineFadeContinueRed = red
    set bj_cineFadeContinueGreen = green
    set bj_cineFadeContinueBlue = blue
    set bj_cineFadeContinueTrans = trans
    set bj_cineFadeContinueDuration = duration
    set bj_cineFadeContinueTex = tex

    // Create a timer to continue the cinematic fade.
    set bj_cineFadeContinueTimer = CreateTimer()
    call TimerStart(bj_cineFadeContinueTimer, duration, false, function ContinueCinematicFadeBJ)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AbortCinematicFadeBJ takes nothing returns nothing
    if (bj_cineFadeContinueTimer != null) then
        call DestroyTimer(bj_cineFadeContinueTimer)
    endif

    if (bj_cineFadeFinishTimer != null) then
        call DestroyTimer(bj_cineFadeFinishTimer)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CinematicFadeBJ takes integer fadetype, real duration, string tex, real red, real green, real blue, real trans returns nothing
    if (fadetype == bj_CINEFADETYPE_FADEOUT) then
        // Fade out to the requested color.
        call AbortCinematicFadeBJ()
        call CinematicFadeCommonBJ(red, green, blue, duration, tex, 100, trans)
    elseif (fadetype == bj_CINEFADETYPE_FADEIN) then
        // Fade in from the requested color.
        call AbortCinematicFadeBJ()
        call CinematicFadeCommonBJ(red, green, blue, duration, tex, trans, 100)
        call FinishCinematicFadeAfterBJ(duration)
    elseif (fadetype == bj_CINEFADETYPE_FADEOUTIN) then
        // Fade out to the requested color, and then fade back in from it.
        if (duration > 0) then
            call AbortCinematicFadeBJ()
            call CinematicFadeCommonBJ(red, green, blue, duration * 0.5, tex, 100, trans)
            call ContinueCinematicFadeAfterBJ(duration * 0.5, red, green, blue, trans, tex)
            call FinishCinematicFadeAfterBJ(duration)
        endif
    else
        // Unrecognized fadetype - ignore the request.
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function CinematicFilterGenericBJ takes real duration, blendmode bmode, string tex, real red0, real green0, real blue0, real trans0, real red1, real green1, real blue1, real trans1 returns nothing
    call AbortCinematicFadeBJ()
    call SetCineFilterTexture(tex)
    call SetCineFilterBlendMode(bmode)
    call SetCineFilterTexMapFlags(TEXMAP_FLAG_NONE)
    call SetCineFilterStartUV(0, 0, 1, 1)
    call SetCineFilterEndUV(0, 0, 1, 1)
    call SetCineFilterStartColor(PercentTo255(red0), PercentTo255(green0), PercentTo255(blue0), PercentTo255(100.0-trans0))
    call SetCineFilterEndColor(PercentTo255(red1), PercentTo255(green1), PercentTo255(blue1), PercentTo255(100.0-trans1))
    call SetCineFilterDuration(duration)
    call DisplayCineFilter(true)
endfunction



//***************************************************************************
//*
//*  Rescuable Unit Utility Functions
//*
//***************************************************************************

//===========================================================================
// Rescues a unit for a player.  This performs the default rescue behavior,
// including a rescue sound, flashing selection circle, ownership change,
// and optionally a unit color change.
//

/**
@patch 1.00
*/
function RescueUnitBJ takes unit whichUnit, player rescuer, boolean changeColor returns nothing
    if IsUnitDeadBJ(whichUnit) or (GetOwningPlayer(whichUnit) == rescuer) then
        return
    endif

    call StartSound(bj_rescueSound)
    call SetUnitOwner(whichUnit, rescuer, changeColor)
    call UnitAddIndicator(whichUnit, 0, 255, 0, 255)
    call PingMinimapForPlayer(rescuer, GetUnitX(whichUnit), GetUnitY(whichUnit), bj_RESCUE_PING_TIME)
endfunction

//===========================================================================

/**
@bug Leaks handle `theUnit`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function TriggerActionUnitRescuedBJ takes nothing returns nothing
    local unit theUnit = GetTriggerUnit()

    if IsUnitType(theUnit, UNIT_TYPE_STRUCTURE) then
        call RescueUnitBJ(theUnit, GetOwningPlayer(GetRescuer()), bj_rescueChangeColorBldg)
    else
        call RescueUnitBJ(theUnit, GetOwningPlayer(GetRescuer()), bj_rescueChangeColorUnit)
    endif
endfunction

//===========================================================================
// Attempt to init triggers for default rescue behavior.  For performance
// reasons, this should only be attempted if a player is set to Rescuable,
// or if a specific unit is thus flagged.
//

/**
@patch 1.00
*/
function TryInitRescuableTriggersBJ takes nothing returns nothing
    local integer index

    if (bj_rescueUnitBehavior == null) then
        set bj_rescueUnitBehavior = CreateTrigger()
        set index = 0
        loop
            call TriggerRegisterPlayerUnitEvent(bj_rescueUnitBehavior, Player(index), EVENT_PLAYER_UNIT_RESCUED, null)
            set index = index + 1
            exitwhen index == bj_MAX_PLAYER_SLOTS
        endloop
        call TriggerAddAction(bj_rescueUnitBehavior, function TriggerActionUnitRescuedBJ)
    endif
endfunction

//===========================================================================
// Determines whether or not rescued units automatically change color upon
// being rescued.
//

/**
@patch 1.00
*/
function SetRescueUnitColorChangeBJ takes boolean changeColor returns nothing
    set bj_rescueChangeColorUnit = changeColor
endfunction

//===========================================================================
// Determines whether or not rescued buildings automatically change color
// upon being rescued.
//

/**
@patch 1.00
*/
function SetRescueBuildingColorChangeBJ takes boolean changeColor returns nothing
    set bj_rescueChangeColorBldg = changeColor
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MakeUnitRescuableToForceBJEnum takes nothing returns nothing
    call TryInitRescuableTriggersBJ()
    call SetUnitRescuable(bj_makeUnitRescuableUnit, GetEnumPlayer(), bj_makeUnitRescuableFlag)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MakeUnitRescuableToForceBJ takes unit whichUnit, boolean isRescuable, force whichForce returns nothing
    // Flag the unit as rescuable/unrescuable for the appropriate players.
    set bj_makeUnitRescuableUnit = whichUnit
    set bj_makeUnitRescuableFlag = isRescuable
    call ForForce(whichForce, function MakeUnitRescuableToForceBJEnum)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function InitRescuableBehaviorBJ takes nothing returns nothing
    local integer index

    set index = 0
    loop
        // If at least one player slot is "Rescuable"-controlled, init the
        // rescue behavior triggers.
        if (GetPlayerController(Player(index)) == MAP_CONTROL_RESCUABLE) then
            call TryInitRescuableTriggersBJ()
            return
        endif
        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction



//***************************************************************************
//*
//*  Research and Upgrade Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerTechResearchedSwap takes integer techid, integer levels, player whichPlayer returns nothing
    call SetPlayerTechResearched(whichPlayer, techid, levels)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerTechMaxAllowedSwap takes integer techid, integer maximum, player whichPlayer returns nothing
    call SetPlayerTechMaxAllowed(whichPlayer, techid, maximum)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SetPlayerMaxHeroesAllowed takes integer maximum, player whichPlayer returns nothing
    call SetPlayerTechMaxAllowed(whichPlayer, 'HERO', maximum)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetPlayerTechCountSimple takes integer techid, player whichPlayer returns integer
    return GetPlayerTechCount(whichPlayer, techid, true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetPlayerTechMaxAllowedSwap takes integer techid, player whichPlayer returns integer
    return GetPlayerTechMaxAllowed(whichPlayer, techid)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerAbilityAvailableBJ takes boolean avail, integer abilid, player whichPlayer returns nothing
    call SetPlayerAbilityAvailable(whichPlayer, abilid, avail)
endfunction



//***************************************************************************
//*
//*  Campaign Utility Functions
//*
//***************************************************************************


/**
@patch 1.00
*/
function SetCampaignMenuRaceBJ takes integer campaignNumber returns nothing
    if (campaignNumber == bj_CAMPAIGN_INDEX_T) then
        call SetCampaignMenuRace(RACE_OTHER)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_H) then
        call SetCampaignMenuRace(RACE_HUMAN)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_U) then
        call SetCampaignMenuRace(RACE_UNDEAD)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_O) then
        call SetCampaignMenuRace(RACE_ORC)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_N) then
        call SetCampaignMenuRace(RACE_NIGHTELF)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_XN) then
        call SetCampaignMenuRaceEx(bj_CAMPAIGN_OFFSET_XN)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_XH) then
        call SetCampaignMenuRaceEx(bj_CAMPAIGN_OFFSET_XH)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_XU) then
        call SetCampaignMenuRaceEx(bj_CAMPAIGN_OFFSET_XU)
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_XO) then
        call SetCampaignMenuRaceEx(bj_CAMPAIGN_OFFSET_XO)
    else
        // Unrecognized campaign - ignore the request
    endif
endfunction

//===========================================================================
// Converts a single campaign mission designation into campaign and mission
// numbers.  The 1000's digit is considered the campaign index, and the 1's
// digit is considered the mission index within that campaign.  This is done
// so that the trigger for this can use a single drop-down to list all of
// the campaign missions.
//

/**
@patch 1.00
*/
function SetMissionAvailableBJ takes boolean available, integer missionIndex returns nothing
    local integer campaignNumber = missionIndex / 1000
    local integer missionNumber = missionIndex - campaignNumber * 1000

    call SetMissionAvailable(campaignNumber, missionNumber, available)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCampaignAvailableBJ takes boolean available, integer campaignNumber returns nothing
    local integer campaignOffset

    if (campaignNumber == bj_CAMPAIGN_INDEX_H) then
        call SetTutorialCleared(true)
    endif

    if (campaignNumber == bj_CAMPAIGN_INDEX_XN) then
        set campaignOffset = bj_CAMPAIGN_OFFSET_XN
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_XH) then
        set campaignOffset = bj_CAMPAIGN_OFFSET_XH
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_XU) then
        set campaignOffset = bj_CAMPAIGN_OFFSET_XU
    elseif (campaignNumber == bj_CAMPAIGN_INDEX_XO) then
        set campaignOffset = bj_CAMPAIGN_OFFSET_XO
    else
        set campaignOffset = campaignNumber
    endif

    call SetCampaignAvailable(campaignOffset, available)
    call SetCampaignMenuRaceBJ(campaignNumber)
    call ForceCampaignSelectScreen()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetCinematicAvailableBJ takes boolean available, integer cinematicIndex returns nothing
    if ( cinematicIndex == bj_CINEMATICINDEX_TOP ) then
        call SetOpCinematicAvailable( bj_CAMPAIGN_INDEX_T, available )
        call PlayCinematic( "TutorialOp" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_HOP) then
        call SetOpCinematicAvailable( bj_CAMPAIGN_INDEX_H, available )
        call PlayCinematic( "HumanOp" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_HED) then
        call SetEdCinematicAvailable( bj_CAMPAIGN_INDEX_H, available )
        call PlayCinematic( "HumanEd" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_OOP) then
        call SetOpCinematicAvailable( bj_CAMPAIGN_INDEX_O, available )
        call PlayCinematic( "OrcOp" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_OED) then
        call SetEdCinematicAvailable( bj_CAMPAIGN_INDEX_O, available )
        call PlayCinematic( "OrcEd" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_UOP) then
        call SetEdCinematicAvailable( bj_CAMPAIGN_INDEX_U, available )
        call PlayCinematic( "UndeadOp" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_UED) then
        call SetEdCinematicAvailable( bj_CAMPAIGN_INDEX_U, available )
        call PlayCinematic( "UndeadEd" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_NOP) then
        call SetEdCinematicAvailable( bj_CAMPAIGN_INDEX_N, available )
        call PlayCinematic( "NightElfOp" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_NED) then
        call SetEdCinematicAvailable( bj_CAMPAIGN_INDEX_N, available )
        call PlayCinematic( "NightElfEd" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_XOP) then
        call SetOpCinematicAvailable( bj_CAMPAIGN_OFFSET_XN, available )
        // call PlayCinematic( "IntroX" )
    elseif (cinematicIndex == bj_CINEMATICINDEX_XED) then
        call SetEdCinematicAvailable( bj_CAMPAIGN_OFFSET_XU, available )
        call PlayCinematic( "OutroX" )
    else
        // Unrecognized cinematic - ignore the request.
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function InitGameCacheBJ takes string campaignFile returns gamecache
    set bj_lastCreatedGameCache = InitGameCache(campaignFile)
    return bj_lastCreatedGameCache
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SaveGameCacheBJ takes gamecache cache returns boolean
    return SaveGameCache(cache)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastCreatedGameCacheBJ takes nothing returns gamecache
    return bj_lastCreatedGameCache
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function InitHashtableBJ takes nothing returns hashtable
    set bj_lastCreatedHashtable = InitHashtable()
    return bj_lastCreatedHashtable
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function GetLastCreatedHashtableBJ takes nothing returns hashtable
    return bj_lastCreatedHashtable
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StoreRealBJ takes real value, string key, string missionKey, gamecache cache returns nothing
    call StoreReal(cache, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StoreIntegerBJ takes integer value, string key, string missionKey, gamecache cache returns nothing
    call StoreInteger(cache, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StoreBooleanBJ takes boolean value, string key, string missionKey, gamecache cache returns nothing
    call StoreBoolean(cache, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function StoreStringBJ takes string value, string key, string missionKey, gamecache cache returns boolean
    return StoreString(cache, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function StoreUnitBJ takes unit whichUnit, string key, string missionKey, gamecache cache returns boolean
    return StoreUnit(cache, missionKey, key, whichUnit)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveRealBJ takes real value, integer key, integer missionKey, hashtable table returns nothing
    call SaveReal(table, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveIntegerBJ takes integer value, integer key, integer missionKey, hashtable table returns nothing
    call SaveInteger(table, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveBooleanBJ takes boolean value, integer key, integer missionKey, hashtable table returns nothing
    call SaveBoolean(table, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveStringBJ takes string value, integer key, integer missionKey, hashtable table returns boolean
    return SaveStr(table, missionKey, key, value)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SavePlayerHandleBJ takes player whichPlayer, integer key, integer missionKey, hashtable table returns boolean
    return SavePlayerHandle(table, missionKey, key, whichPlayer)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveWidgetHandleBJ takes widget whichWidget, integer key, integer missionKey, hashtable table returns boolean
    return SaveWidgetHandle(table, missionKey, key, whichWidget)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveDestructableHandleBJ takes destructable whichDestructable, integer key, integer missionKey, hashtable table returns boolean
    return SaveDestructableHandle(table, missionKey, key, whichDestructable)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveItemHandleBJ takes item whichItem, integer key, integer missionKey, hashtable table returns boolean
    return SaveItemHandle(table, missionKey, key, whichItem)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveUnitHandleBJ takes unit whichUnit, integer key, integer missionKey, hashtable table returns boolean
    return SaveUnitHandle(table, missionKey, key, whichUnit)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveAbilityHandleBJ takes ability whichAbility, integer key, integer missionKey, hashtable table returns boolean
    return SaveAbilityHandle(table, missionKey, key, whichAbility)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTimerHandleBJ takes timer whichTimer, integer key, integer missionKey, hashtable table returns boolean
    return SaveTimerHandle(table, missionKey, key, whichTimer)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTriggerHandleBJ takes trigger whichTrigger, integer key, integer missionKey, hashtable table returns boolean
    return SaveTriggerHandle(table, missionKey, key, whichTrigger)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTriggerConditionHandleBJ takes triggercondition whichTriggercondition, integer key, integer missionKey, hashtable table returns boolean
    return SaveTriggerConditionHandle(table, missionKey, key, whichTriggercondition)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTriggerActionHandleBJ takes triggeraction whichTriggeraction, integer key, integer missionKey, hashtable table returns boolean
    return SaveTriggerActionHandle(table, missionKey, key, whichTriggeraction)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTriggerEventHandleBJ takes event whichEvent, integer key, integer missionKey, hashtable table returns boolean
    return SaveTriggerEventHandle(table, missionKey, key, whichEvent)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveForceHandleBJ takes force whichForce, integer key, integer missionKey, hashtable table returns boolean
    return SaveForceHandle(table, missionKey, key, whichForce)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveGroupHandleBJ takes group whichGroup, integer key, integer missionKey, hashtable table returns boolean
    return SaveGroupHandle(table, missionKey, key, whichGroup)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveLocationHandleBJ takes location whichLocation, integer key, integer missionKey, hashtable table returns boolean
    return SaveLocationHandle(table, missionKey, key, whichLocation)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveRectHandleBJ takes rect whichRect, integer key, integer missionKey, hashtable table returns boolean
    return SaveRectHandle(table, missionKey, key, whichRect)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveBooleanExprHandleBJ takes boolexpr whichBoolexpr, integer key, integer missionKey, hashtable table returns boolean
    return SaveBooleanExprHandle(table, missionKey, key, whichBoolexpr)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveSoundHandleBJ takes sound whichSound, integer key, integer missionKey, hashtable table returns boolean
    return SaveSoundHandle(table, missionKey, key, whichSound)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveEffectHandleBJ takes effect whichEffect, integer key, integer missionKey, hashtable table returns boolean
    return SaveEffectHandle(table, missionKey, key, whichEffect)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveUnitPoolHandleBJ takes unitpool whichUnitpool, integer key, integer missionKey, hashtable table returns boolean
    return SaveUnitPoolHandle(table, missionKey, key, whichUnitpool)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveItemPoolHandleBJ takes itempool whichItempool, integer key, integer missionKey, hashtable table returns boolean
    return SaveItemPoolHandle(table, missionKey, key, whichItempool)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveQuestHandleBJ takes quest whichQuest, integer key, integer missionKey, hashtable table returns boolean
    return SaveQuestHandle(table, missionKey, key, whichQuest)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveQuestItemHandleBJ takes questitem whichQuestitem, integer key, integer missionKey, hashtable table returns boolean
    return SaveQuestItemHandle(table, missionKey, key, whichQuestitem)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveDefeatConditionHandleBJ takes defeatcondition whichDefeatcondition, integer key, integer missionKey, hashtable table returns boolean
    return SaveDefeatConditionHandle(table, missionKey, key, whichDefeatcondition)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTimerDialogHandleBJ takes timerdialog whichTimerdialog, integer key, integer missionKey, hashtable table returns boolean
    return SaveTimerDialogHandle(table, missionKey, key, whichTimerdialog)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveLeaderboardHandleBJ takes leaderboard whichLeaderboard, integer key, integer missionKey, hashtable table returns boolean
    return SaveLeaderboardHandle(table, missionKey, key, whichLeaderboard)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveMultiboardHandleBJ takes multiboard whichMultiboard, integer key, integer missionKey, hashtable table returns boolean
    return SaveMultiboardHandle(table, missionKey, key, whichMultiboard)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveMultiboardItemHandleBJ takes multiboarditem whichMultiboarditem, integer key, integer missionKey, hashtable table returns boolean
    return SaveMultiboardItemHandle(table, missionKey, key, whichMultiboarditem)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTrackableHandleBJ takes trackable whichTrackable, integer key, integer missionKey, hashtable table returns boolean
    return SaveTrackableHandle(table, missionKey, key, whichTrackable)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveDialogHandleBJ takes dialog whichDialog, integer key, integer missionKey, hashtable table returns boolean
    return SaveDialogHandle(table, missionKey, key, whichDialog)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveButtonHandleBJ takes button whichButton, integer key, integer missionKey, hashtable table returns boolean
    return SaveButtonHandle(table, missionKey, key, whichButton)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveTextTagHandleBJ takes texttag whichTexttag, integer key, integer missionKey, hashtable table returns boolean
    return SaveTextTagHandle(table, missionKey, key, whichTexttag)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveLightningHandleBJ takes lightning whichLightning, integer key, integer missionKey, hashtable table returns boolean
    return SaveLightningHandle(table, missionKey, key, whichLightning)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveImageHandleBJ takes image whichImage, integer key, integer missionKey, hashtable table returns boolean
    return SaveImageHandle(table, missionKey, key, whichImage)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveUbersplatHandleBJ takes ubersplat whichUbersplat, integer key, integer missionKey, hashtable table returns boolean
    return SaveUbersplatHandle(table, missionKey, key, whichUbersplat)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveRegionHandleBJ takes region whichRegion, integer key, integer missionKey, hashtable table returns boolean
    return SaveRegionHandle(table, missionKey, key, whichRegion)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveFogStateHandleBJ takes fogstate whichFogState, integer key, integer missionKey, hashtable table returns boolean
    return SaveFogStateHandle(table, missionKey, key, whichFogState)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveFogModifierHandleBJ takes fogmodifier whichFogModifier, integer key, integer missionKey, hashtable table returns boolean
    return SaveFogModifierHandle(table, missionKey, key, whichFogModifier)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveAgentHandleBJ takes agent whichAgent, integer key, integer missionKey, hashtable table returns boolean
    return SaveAgentHandle(table, missionKey, key, whichAgent)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function SaveHashtableHandleBJ takes hashtable whichHashtable, integer key, integer missionKey, hashtable table returns boolean
    return SaveHashtableHandle(table, missionKey, key, whichHashtable)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetStoredRealBJ takes string key, string missionKey, gamecache cache returns real
    //call SyncStoredReal(cache, missionKey, key)
    return GetStoredReal(cache, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetStoredIntegerBJ takes string key, string missionKey, gamecache cache returns integer
    //call SyncStoredInteger(cache, missionKey, key)
    return GetStoredInteger(cache, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetStoredBooleanBJ takes string key, string missionKey, gamecache cache returns boolean
    //call SyncStoredBoolean(cache, missionKey, key)
    return GetStoredBoolean(cache, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function GetStoredStringBJ takes string key, string missionKey, gamecache cache returns string
    local string s

    //call SyncStoredString(cache, missionKey, key)
    set s = GetStoredString(cache, missionKey, key)
    if (s == null) then
        return ""
    else
        return s
    endif
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadRealBJ takes integer key, integer missionKey, hashtable table returns real
    //call SyncStoredReal(table, missionKey, key)
    return LoadReal(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadIntegerBJ takes integer key, integer missionKey, hashtable table returns integer
    //call SyncStoredInteger(table, missionKey, key)
    return LoadInteger(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadBooleanBJ takes integer key, integer missionKey, hashtable table returns boolean
    //call SyncStoredBoolean(table, missionKey, key)
    return LoadBoolean(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadStringBJ takes integer key, integer missionKey, hashtable table returns string
    local string s

    //call SyncStoredString(table, missionKey, key)
    set s = LoadStr(table, missionKey, key)
    if (s == null) then
        return ""
    else
        return s
    endif
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadPlayerHandleBJ takes integer key, integer missionKey, hashtable table returns player
    return LoadPlayerHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadWidgetHandleBJ takes integer key, integer missionKey, hashtable table returns widget
    return LoadWidgetHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadDestructableHandleBJ takes integer key, integer missionKey, hashtable table returns destructable
    return LoadDestructableHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadItemHandleBJ takes integer key, integer missionKey, hashtable table returns item
    return LoadItemHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadUnitHandleBJ takes integer key, integer missionKey, hashtable table returns unit
    return LoadUnitHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadAbilityHandleBJ takes integer key, integer missionKey, hashtable table returns ability
    return LoadAbilityHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTimerHandleBJ takes integer key, integer missionKey, hashtable table returns timer
    return LoadTimerHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTriggerHandleBJ takes integer key, integer missionKey, hashtable table returns trigger
    return LoadTriggerHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTriggerConditionHandleBJ takes integer key, integer missionKey, hashtable table returns triggercondition
    return LoadTriggerConditionHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTriggerActionHandleBJ takes integer key, integer missionKey, hashtable table returns triggeraction
    return LoadTriggerActionHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTriggerEventHandleBJ takes integer key, integer missionKey, hashtable table returns event
    return LoadTriggerEventHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadForceHandleBJ takes integer key, integer missionKey, hashtable table returns force
    return LoadForceHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadGroupHandleBJ takes integer key, integer missionKey, hashtable table returns group
    return LoadGroupHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadLocationHandleBJ takes integer key, integer missionKey, hashtable table returns location
    return LoadLocationHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadRectHandleBJ takes integer key, integer missionKey, hashtable table returns rect
    return LoadRectHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadBooleanExprHandleBJ takes integer key, integer missionKey, hashtable table returns boolexpr
    return LoadBooleanExprHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadSoundHandleBJ takes integer key, integer missionKey, hashtable table returns sound
    return LoadSoundHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadEffectHandleBJ takes integer key, integer missionKey, hashtable table returns effect
    return LoadEffectHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadUnitPoolHandleBJ takes integer key, integer missionKey, hashtable table returns unitpool
    return LoadUnitPoolHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadItemPoolHandleBJ takes integer key, integer missionKey, hashtable table returns itempool
    return LoadItemPoolHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadQuestHandleBJ takes integer key, integer missionKey, hashtable table returns quest
    return LoadQuestHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadQuestItemHandleBJ takes integer key, integer missionKey, hashtable table returns questitem
    return LoadQuestItemHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadDefeatConditionHandleBJ takes integer key, integer missionKey, hashtable table returns defeatcondition
    return LoadDefeatConditionHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTimerDialogHandleBJ takes integer key, integer missionKey, hashtable table returns timerdialog
    return LoadTimerDialogHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadLeaderboardHandleBJ takes integer key, integer missionKey, hashtable table returns leaderboard
    return LoadLeaderboardHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadMultiboardHandleBJ takes integer key, integer missionKey, hashtable table returns multiboard
    return LoadMultiboardHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadMultiboardItemHandleBJ takes integer key, integer missionKey, hashtable table returns multiboarditem
    return LoadMultiboardItemHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTrackableHandleBJ takes integer key, integer missionKey, hashtable table returns trackable
    return LoadTrackableHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadDialogHandleBJ takes integer key, integer missionKey, hashtable table returns dialog
    return LoadDialogHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadButtonHandleBJ takes integer key, integer missionKey, hashtable table returns button
    return LoadButtonHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadTextTagHandleBJ takes integer key, integer missionKey, hashtable table returns texttag
    return LoadTextTagHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadLightningHandleBJ takes integer key, integer missionKey, hashtable table returns lightning
    return LoadLightningHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadImageHandleBJ takes integer key, integer missionKey, hashtable table returns image
    return LoadImageHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadUbersplatHandleBJ takes integer key, integer missionKey, hashtable table returns ubersplat
    return LoadUbersplatHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadRegionHandleBJ takes integer key, integer missionKey, hashtable table returns region
    return LoadRegionHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadFogStateHandleBJ takes integer key, integer missionKey, hashtable table returns fogstate
    return LoadFogStateHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadFogModifierHandleBJ takes integer key, integer missionKey, hashtable table returns fogmodifier
    return LoadFogModifierHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function LoadHashtableHandleBJ takes integer key, integer missionKey, hashtable table returns hashtable
    return LoadHashtableHandle(table, missionKey, key)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RestoreUnitLocFacingAngleBJ takes string key, string missionKey, gamecache cache, player forWhichPlayer, location loc, real facing returns unit
    //call SyncStoredUnit(cache, missionKey, key)
    set bj_lastLoadedUnit = RestoreUnit(cache, missionKey, key, forWhichPlayer, GetLocationX(loc), GetLocationY(loc), facing)
    return bj_lastLoadedUnit
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RestoreUnitLocFacingPointBJ takes string key, string missionKey, gamecache cache, player forWhichPlayer, location loc, location lookAt returns unit
    //call SyncStoredUnit(cache, missionKey, key)
    return RestoreUnitLocFacingAngleBJ(key, missionKey, cache, forWhichPlayer, loc, AngleBetweenPoints(loc, lookAt))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastRestoredUnitBJ takes nothing returns unit
    return bj_lastLoadedUnit
endfunction

//===========================================================================

/**
@patch 1.07
*/
function FlushGameCacheBJ takes gamecache cache returns nothing
    call FlushGameCache(cache)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function FlushStoredMissionBJ takes string missionKey, gamecache cache returns nothing
    call FlushStoredMission(cache, missionKey)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function FlushParentHashtableBJ takes hashtable table returns nothing
    call FlushParentHashtable(table)
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function FlushChildHashtableBJ takes integer missionKey, hashtable table returns nothing
    call FlushChildHashtable(table, missionKey)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function HaveStoredValue takes string key, integer valueType, string missionKey, gamecache cache returns boolean
    if (valueType == bj_GAMECACHE_BOOLEAN) then
        return HaveStoredBoolean(cache, missionKey, key)
    elseif (valueType == bj_GAMECACHE_INTEGER) then
        return HaveStoredInteger(cache, missionKey, key)
    elseif (valueType == bj_GAMECACHE_REAL) then
        return HaveStoredReal(cache, missionKey, key)
    elseif (valueType == bj_GAMECACHE_UNIT) then
        return HaveStoredUnit(cache, missionKey, key)
    elseif (valueType == bj_GAMECACHE_STRING) then
        return HaveStoredString(cache, missionKey, key)
    else
        // Unrecognized value type - ignore the request.
        return false
    endif
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function HaveSavedValue takes integer key, integer valueType, integer missionKey, hashtable table returns boolean
    if (valueType == bj_HASHTABLE_BOOLEAN) then
        return HaveSavedBoolean(table, missionKey, key)
    elseif (valueType == bj_HASHTABLE_INTEGER) then
        return HaveSavedInteger(table, missionKey, key)
    elseif (valueType == bj_HASHTABLE_REAL) then
        return HaveSavedReal(table, missionKey, key)
    elseif (valueType == bj_HASHTABLE_STRING) then
        return HaveSavedString(table, missionKey, key)
    elseif (valueType == bj_HASHTABLE_HANDLE) then
        return HaveSavedHandle(table, missionKey, key)
    else
        // Unrecognized value type - ignore the request.
        return false
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function ShowCustomCampaignButton takes boolean show, integer whichButton returns nothing
    call SetCustomCampaignButtonVisible(whichButton - 1, show)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function IsCustomCampaignButtonVisibile takes integer whichButton returns boolean
    return GetCustomCampaignButtonVisible(whichButton - 1)
endfunction

//===========================================================================
// Placeholder function for auto save feature
//===========================================================================

/**
@patch 1.32.0.13369
*/
function SaveGameCheckPointBJ takes string mapSaveName, boolean doCheckpointHint returns nothing
	call SaveGameCheckpoint(mapSaveName, doCheckpointHint)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function LoadGameBJ takes string loadFileName, boolean doScoreScreen returns nothing
    call LoadGame(loadFileName, doScoreScreen)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SaveAndChangeLevelBJ takes string saveFileName, string newLevel, boolean doScoreScreen returns nothing
    call SaveGame(saveFileName)
    call ChangeLevel(newLevel, doScoreScreen)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function SaveAndLoadGameBJ takes string saveFileName, string loadFileName, boolean doScoreScreen returns nothing
    call SaveGame(saveFileName)
    call LoadGame(loadFileName, doScoreScreen)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RenameSaveDirectoryBJ takes string sourceDirName, string destDirName returns boolean
    return RenameSaveDirectory(sourceDirName, destDirName)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RemoveSaveDirectoryBJ takes string sourceDirName returns boolean
    return RemoveSaveDirectory(sourceDirName)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function CopySaveGameBJ takes string sourceSaveName, string destSaveName returns boolean
    return CopySaveGame(sourceSaveName, destSaveName)
endfunction



//***************************************************************************
//*
//*  Miscellaneous Utility Functions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function GetPlayerStartLocationX takes player whichPlayer returns real
    return GetStartLocationX(GetPlayerStartLocation(whichPlayer))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetPlayerStartLocationY takes player whichPlayer returns real
    return GetStartLocationY(GetPlayerStartLocation(whichPlayer))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetPlayerStartLocationLoc takes player whichPlayer returns location
    return GetStartLocationLoc(GetPlayerStartLocation(whichPlayer))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetRectCenter takes rect whichRect returns location
    return Location(GetRectCenterX(whichRect), GetRectCenterY(whichRect))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsPlayerSlotState takes player whichPlayer, playerslotstate whichState returns boolean
    return GetPlayerSlotState(whichPlayer) == whichState
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetFadeFromSeconds takes real seconds returns integer
    if (seconds != 0) then
        return 128 / R2I(seconds)
    endif
    return 10000
endfunction

//===========================================================================

/**
@patch 1.24a
*/
function GetFadeFromSecondsAsReal takes real seconds returns real
    if (seconds != 0) then
        return 128.00 / seconds
    endif
    return 10000.00
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AdjustPlayerStateSimpleBJ takes player whichPlayer, playerstate whichPlayerState, integer delta returns nothing
    call SetPlayerState(whichPlayer, whichPlayerState, GetPlayerState(whichPlayer, whichPlayerState) + delta)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AdjustPlayerStateBJ takes integer delta, player whichPlayer, playerstate whichPlayerState returns nothing
    // If the change was positive, apply the difference to the player's
    // gathered resources property as well.
    if (delta > 0) then
        if (whichPlayerState == PLAYER_STATE_RESOURCE_GOLD) then
            call AdjustPlayerStateSimpleBJ(whichPlayer, PLAYER_STATE_GOLD_GATHERED, delta)
        elseif (whichPlayerState == PLAYER_STATE_RESOURCE_LUMBER) then
            call AdjustPlayerStateSimpleBJ(whichPlayer, PLAYER_STATE_LUMBER_GATHERED, delta)
        endif
    endif

    call AdjustPlayerStateSimpleBJ(whichPlayer, whichPlayerState, delta)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerStateBJ takes player whichPlayer, playerstate whichPlayerState, integer value returns nothing
    local integer oldValue = GetPlayerState(whichPlayer, whichPlayerState)
    call AdjustPlayerStateBJ(value - oldValue, whichPlayer, whichPlayerState)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerFlagBJ takes playerstate whichPlayerFlag, boolean flag, player whichPlayer returns nothing
    call SetPlayerState(whichPlayer, whichPlayerFlag, IntegerTertiaryOp(flag, 1, 0))
endfunction

//===========================================================================

/**
@patch 1.15
*/
function SetPlayerTaxRateBJ takes integer rate, playerstate whichResource, player sourcePlayer, player otherPlayer returns nothing
    call SetPlayerTaxRate(sourcePlayer, otherPlayer, whichResource, rate)
endfunction

//===========================================================================

/**
@patch 1.15
*/
function GetPlayerTaxRateBJ takes playerstate whichResource, player sourcePlayer, player otherPlayer returns integer
    return GetPlayerTaxRate(sourcePlayer, otherPlayer, whichResource)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IsPlayerFlagSetBJ takes playerstate whichPlayerFlag, player whichPlayer returns boolean
    return GetPlayerState(whichPlayer, whichPlayerFlag) == 1
endfunction

//===========================================================================

/**
@patch 1.00
*/
function AddResourceAmountBJ takes integer delta, unit whichUnit returns nothing
    call AddResourceAmount(whichUnit, delta)
endfunction

//===========================================================================

/**
returns WorldEdit-type player ID for player (these start with 1; e.g. player red is 1).

@param whichPlayer Target player

@note For zero-based IDs see: `GetPlayerId`, `Player`

@patch 1.00
*/
function GetConvertedPlayerId takes player whichPlayer returns integer
    return GetPlayerId(whichPlayer) + 1
endfunction

//===========================================================================

/**
@patch 1.00
*/
function ConvertedPlayer takes integer convertedPlayerId returns player
    return Player(convertedPlayerId - 1)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetRectWidthBJ takes rect r returns real
    return GetRectMaxX(r) - GetRectMinX(r)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetRectHeightBJ takes rect r returns real
    return GetRectMaxY(r) - GetRectMinY(r)
endfunction

//===========================================================================
// Replaces a gold mine with a blighted gold mine for the given player.
//

/**
@bug Leaks handle `newMine`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function BlightGoldMineForPlayerBJ takes unit goldMine, player whichPlayer returns unit
    local real    mineX
    local real    mineY
    local integer mineGold
    local unit    newMine

    // Make sure we're replacing a Gold Mine and not some other type of unit.
    if GetUnitTypeId(goldMine) != 'ngol' then
        return null
    endif

    // Save the Gold Mine's properties and remove it.
    set mineX    = GetUnitX(goldMine)
    set mineY    = GetUnitY(goldMine)
    set mineGold = GetResourceAmount(goldMine)
    call RemoveUnit(goldMine)

    // Create a Haunted Gold Mine to replace the Gold Mine.
    set newMine = CreateBlightedGoldmine(whichPlayer, mineX, mineY, bj_UNIT_FACING)
    call SetResourceAmount(newMine, mineGold)
    return newMine
endfunction

//===========================================================================

/**
@patch 1.00
*/
function BlightGoldMineForPlayer takes unit goldMine, player whichPlayer returns unit
    set bj_lastHauntedGoldMine = BlightGoldMineForPlayerBJ(goldMine, whichPlayer)
    return bj_lastHauntedGoldMine
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetLastHauntedGoldMine takes nothing returns unit
    return bj_lastHauntedGoldMine
endfunction

//===========================================================================

/**
@patch 1.13
*/
function IsPointBlightedBJ takes location where returns boolean
    return IsPointBlighted(GetLocationX(where), GetLocationY(where))
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerColorBJEnum takes nothing returns nothing
    call SetUnitColor(GetEnumUnit(), bj_setPlayerTargetColor)
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function SetPlayerColorBJ takes player whichPlayer, playercolor color, boolean changeExisting returns nothing
    local group g

    call SetPlayerColor(whichPlayer, color)
    if changeExisting then
        set bj_setPlayerTargetColor = color
        set g = CreateGroup()
        call GroupEnumUnitsOfPlayer(g, whichPlayer, null)
        call ForGroup(g, function SetPlayerColorBJEnum)
        call DestroyGroup(g)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetPlayerUnitAvailableBJ takes integer unitId, boolean allowed, player whichPlayer returns nothing
    if allowed then
        call SetPlayerTechMaxAllowed(whichPlayer, unitId, -1)
    else
        call SetPlayerTechMaxAllowed(whichPlayer, unitId, 0)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function LockGameSpeedBJ takes nothing returns nothing
    call SetMapFlag(MAP_LOCK_SPEED, true)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function UnlockGameSpeedBJ takes nothing returns nothing
    call SetMapFlag(MAP_LOCK_SPEED, false)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IssueTargetOrderBJ takes unit whichUnit, string order, widget targetWidget returns boolean
    return IssueTargetOrder( whichUnit, order, targetWidget )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IssuePointOrderLocBJ takes unit whichUnit, string order, location whichLocation returns boolean
    return IssuePointOrderLoc( whichUnit, order, whichLocation )
endfunction

//===========================================================================
// Two distinct trigger actions can't share the same function name, so this
// dummy function simply mimics the behavior of an existing call.
//

/**
@patch 1.00
*/
function IssueTargetDestructableOrder takes unit whichUnit, string order, widget targetWidget returns boolean
    return IssueTargetOrder( whichUnit, order, targetWidget )
endfunction


/**
@patch 1.15
*/
function IssueTargetItemOrder takes unit whichUnit, string order, widget targetWidget returns boolean
    return IssueTargetOrder( whichUnit, order, targetWidget )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function IssueImmediateOrderBJ takes unit whichUnit, string order returns boolean
    return IssueImmediateOrder( whichUnit, order )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GroupTargetOrderBJ takes group whichGroup, string order, widget targetWidget returns boolean
    return GroupTargetOrder( whichGroup, order, targetWidget )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GroupPointOrderLocBJ takes group whichGroup, string order, location whichLocation returns boolean
    return GroupPointOrderLoc( whichGroup, order, whichLocation )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GroupImmediateOrderBJ takes group whichGroup, string order returns boolean
    return GroupImmediateOrder( whichGroup, order )
endfunction

//===========================================================================
// Two distinct trigger actions can't share the same function name, so this
// dummy function simply mimics the behavior of an existing call.
//

/**
@patch 1.00
*/
function GroupTargetDestructableOrder takes group whichGroup, string order, widget targetWidget returns boolean
    return GroupTargetOrder( whichGroup, order, targetWidget )
endfunction


/**
@patch 1.15
*/
function GroupTargetItemOrder takes group whichGroup, string order, widget targetWidget returns boolean
    return GroupTargetOrder( whichGroup, order, targetWidget )
endfunction

//===========================================================================

/**
@patch 1.00
*/
function GetDyingDestructable takes nothing returns destructable
    return GetTriggerDestructable()
endfunction

//===========================================================================
// Rally point setting
//

/**
@patch 1.13
*/
function SetUnitRallyPoint takes unit whichUnit, location targPos returns nothing
    call IssuePointOrderLocBJ(whichUnit, "setrally", targPos)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function SetUnitRallyUnit takes unit whichUnit, unit targUnit returns nothing
    call IssueTargetOrder(whichUnit, "setrally", targUnit)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function SetUnitRallyDestructable takes unit whichUnit, destructable targDest returns nothing
    call IssueTargetOrder(whichUnit, "setrally", targDest)
endfunction

//===========================================================================
// Utility function for use by editor-generated item drop table triggers.
// This function is added as an action to all destructable drop triggers,
// so that a widget drop may be differentiated from a unit drop.
//

/**
@patch 1.07
*/
function SaveDyingWidget takes nothing returns nothing
    set bj_lastDyingWidget = GetTriggerWidget()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetBlightRectBJ takes boolean addBlight, player whichPlayer, rect r returns nothing
    call SetBlightRect(whichPlayer, r, addBlight)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetBlightRadiusLocBJ takes boolean addBlight, player whichPlayer, location loc, real radius returns nothing
    call SetBlightLoc(whichPlayer, loc, radius, addBlight)
endfunction

//===========================================================================

/**
@patch 1.13
*/
function GetAbilityName takes integer abilcode returns string
    return GetObjectName(abilcode)
endfunction


//***************************************************************************
//*
//*  Melee Template Visibility Settings
//*
//***************************************************************************

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@patch 1.00
*/
function MeleeStartingVisibility takes nothing returns nothing
    // Start by setting the ToD.
    call SetFloatGameState(GAME_STATE_TIME_OF_DAY, bj_MELEE_STARTING_TOD)

    // call FogMaskEnable(true)
    // call FogEnable(true)
endfunction



//***************************************************************************
//*
//*  Melee Template Starting Resources
//*
//***************************************************************************

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@bug Leaks handle `v`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeStartingResources takes nothing returns nothing
    local integer index
    local player  indexPlayer
    local version v
    local integer startingGold
    local integer startingLumber

    set v = VersionGet()
    if (v == VERSION_REIGN_OF_CHAOS) then
        set startingGold = bj_MELEE_STARTING_GOLD_V0
        set startingLumber = bj_MELEE_STARTING_LUMBER_V0
    else
        set startingGold = bj_MELEE_STARTING_GOLD_V1
        set startingLumber = bj_MELEE_STARTING_LUMBER_V1
    endif

    // Set each player's starting resources.
    set index = 0
    loop
        set indexPlayer = Player(index)
        if (GetPlayerSlotState(indexPlayer) == PLAYER_SLOT_STATE_PLAYING) then
            call SetPlayerState(indexPlayer, PLAYER_STATE_RESOURCE_GOLD, startingGold)
            call SetPlayerState(indexPlayer, PLAYER_STATE_RESOURCE_LUMBER, startingLumber)
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction



//***************************************************************************
//*
//*  Melee Template Hero Limit
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function ReducePlayerTechMaxAllowed takes player whichPlayer, integer techId, integer limit returns nothing
    local integer oldMax = GetPlayerTechMaxAllowed(whichPlayer, techId)

    // A value of -1 is used to indicate no limit, so check for that as well.
    if (oldMax < 0 or oldMax > limit) then
        call SetPlayerTechMaxAllowed(whichPlayer, techId, limit)
    endif
endfunction

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@patch 1.00
*/
function MeleeStartingHeroLimit takes nothing returns nothing
    local integer index

    set index = 0
    loop
        // max heroes per player
        call SetPlayerMaxHeroesAllowed(bj_MELEE_HERO_LIMIT, Player(index))

        // each player is restricted to a limit per hero type as well
        call ReducePlayerTechMaxAllowed(Player(index), 'Hamg', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Hmkg', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Hpal', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Hblm', bj_MELEE_HERO_TYPE_LIMIT)

        call ReducePlayerTechMaxAllowed(Player(index), 'Obla', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Ofar', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Otch', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Oshd', bj_MELEE_HERO_TYPE_LIMIT)

        call ReducePlayerTechMaxAllowed(Player(index), 'Edem', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Ekee', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Emoo', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Ewar', bj_MELEE_HERO_TYPE_LIMIT)

        call ReducePlayerTechMaxAllowed(Player(index), 'Udea', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Udre', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Ulic', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Ucrl', bj_MELEE_HERO_TYPE_LIMIT)

        call ReducePlayerTechMaxAllowed(Player(index), 'Npbm', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Nbrn', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Nngs', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Nplh', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Nbst', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Nalc', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Ntin', bj_MELEE_HERO_TYPE_LIMIT)
        call ReducePlayerTechMaxAllowed(Player(index), 'Nfir', bj_MELEE_HERO_TYPE_LIMIT)

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction



//***************************************************************************
//*
//*  Melee Template Granted Hero Items
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function MeleeTrainedUnitIsHeroBJFilter takes nothing returns boolean
    return IsUnitType(GetFilterUnit(), UNIT_TYPE_HERO)
endfunction

//===========================================================================
// The first N heroes trained or hired for each player start off with a
// standard set of items.  This is currently:
//   - 1x Scroll of Town Portal
//

/**
@patch 1.00
*/
function MeleeGrantItemsToHero takes unit whichUnit returns nothing
    local integer owner   = GetPlayerId(GetOwningPlayer(whichUnit))

    // If we haven't twinked N heroes for this player yet, twink away.
    if (bj_meleeTwinkedHeroes[owner] < bj_MELEE_MAX_TWINKED_HEROES) then
        call UnitAddItemById(whichUnit, 'stwp')
        set bj_meleeTwinkedHeroes[owner] = bj_meleeTwinkedHeroes[owner] + 1
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MeleeGrantItemsToTrainedHero takes nothing returns nothing
    call MeleeGrantItemsToHero(GetTrainedUnit())
endfunction

//===========================================================================

/**
@patch 1.07
*/
function MeleeGrantItemsToHiredHero takes nothing returns nothing
    call MeleeGrantItemsToHero(GetSoldUnit())
endfunction

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@patch 1.00
*/
function MeleeGrantHeroItems takes nothing returns nothing
    local integer index
    local trigger trig

    // Initialize the twinked hero counts.
    set index = 0
    loop
        set bj_meleeTwinkedHeroes[index] = 0

        set index = index + 1
        exitwhen index == bj_MAX_PLAYER_SLOTS
    endloop

    // Register for an event whenever a hero is trained, so that we can give
    // him/her their starting items.
    set index = 0
    loop
        set trig = CreateTrigger()
        call TriggerRegisterPlayerUnitEvent(trig, Player(index), EVENT_PLAYER_UNIT_TRAIN_FINISH, filterMeleeTrainedUnitIsHeroBJ)
        call TriggerAddAction(trig, function MeleeGrantItemsToTrainedHero)

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop

    // Register for an event whenever a neutral hero is hired, so that we
    // can give him/her their starting items.
    set trig = CreateTrigger()
    call TriggerRegisterPlayerUnitEvent(trig, Player(PLAYER_NEUTRAL_PASSIVE), EVENT_PLAYER_UNIT_SELL, filterMeleeTrainedUnitIsHeroBJ)
    call TriggerAddAction(trig, function MeleeGrantItemsToHiredHero)

    // Flag that we are giving starting items to heroes, so that the melee
    // starting units code can create them as necessary.
    set bj_meleeGrantHeroItems = true
endfunction



//***************************************************************************
//*
//*  Melee Template Clear Start Locations
//*
//***************************************************************************

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@bug Leaks handle `theUnit`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeClearExcessUnit takes nothing returns nothing
    local unit    theUnit = GetEnumUnit()
    local integer owner   = GetPlayerId(GetOwningPlayer(theUnit))

    if (owner == PLAYER_NEUTRAL_AGGRESSIVE) then
        // Remove any Neutral Hostile units from the area.
        call RemoveUnit(GetEnumUnit())
    elseif (owner == PLAYER_NEUTRAL_PASSIVE) then
        // Remove non-structure Neutral Passive units from the area.
        if not IsUnitType(theUnit, UNIT_TYPE_STRUCTURE) then
            call RemoveUnit(GetEnumUnit())
        endif
    endif
endfunction

//===========================================================================

/**
@bug Leaks handle `nearbyUnits`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeClearNearbyUnits takes real x, real y, real range returns nothing
    local group nearbyUnits
    
    set nearbyUnits = CreateGroup()
    call GroupEnumUnitsInRange(nearbyUnits, x, y, range, null)
    call ForGroup(nearbyUnits, function MeleeClearExcessUnit)
    call DestroyGroup(nearbyUnits)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MeleeClearExcessUnits takes nothing returns nothing
    local integer index
    local real    locX
    local real    locY
    local player  indexPlayer

    set index = 0
    loop
        set indexPlayer = Player(index)

        // If the player slot is being used, clear any nearby creeps.
        if (GetPlayerSlotState(indexPlayer) == PLAYER_SLOT_STATE_PLAYING) then
            set locX = GetStartLocationX(GetPlayerStartLocation(indexPlayer))
            set locY = GetStartLocationY(GetPlayerStartLocation(indexPlayer))

            call MeleeClearNearbyUnits(locX, locY, bj_MELEE_CLEAR_UNITS_RADIUS)
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction



//***************************************************************************
//*
//*  Melee Template Starting Units
//*
//***************************************************************************

//===========================================================================

/**
@bug Leaks handle `enumUnit`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `unitLoc`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeEnumFindNearestMine takes nothing returns nothing
    local unit enumUnit = GetEnumUnit()
    local real dist
    local location unitLoc

    if (GetUnitTypeId(enumUnit) == 'ngol') then
        set unitLoc = GetUnitLoc(enumUnit)
        set dist = DistanceBetweenPoints(unitLoc, bj_meleeNearestMineToLoc)
        call RemoveLocation(unitLoc)

        // If this is our first mine, or the closest thusfar, use it instead.
        if (bj_meleeNearestMineDist < 0) or (dist < bj_meleeNearestMineDist) then
            set bj_meleeNearestMine = enumUnit
            set bj_meleeNearestMineDist = dist
        endif
    endif
endfunction

//===========================================================================

/**
@bug Leaks handle `nearbyMines`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeFindNearestMine takes location src, real range returns unit
    local group nearbyMines

    set bj_meleeNearestMine = null
    set bj_meleeNearestMineDist = -1
    set bj_meleeNearestMineToLoc = src

    set nearbyMines = CreateGroup()
    call GroupEnumUnitsInRangeOfLoc(nearbyMines, src, range, null)
    call ForGroup(nearbyMines, function MeleeEnumFindNearestMine)
    call DestroyGroup(nearbyMines)

    return bj_meleeNearestMine
endfunction

//===========================================================================

/**
@note It does not touch the passed `loc` location, you must remove it manually to avoid leaks.

@bug Leaks handle `hero`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `v`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeRandomHeroLoc takes player p, integer id1, integer id2, integer id3, integer id4, location loc returns unit
    local unit    hero = null
    local integer roll
    local integer pick
    local version v

    // The selection of heroes is dependant on the game version.
    set v = VersionGet()
    if (v == VERSION_REIGN_OF_CHAOS) then
        set roll = GetRandomInt(1,3)
    else
        set roll = GetRandomInt(1,4)
    endif

    // Translate the roll into a unitid.
    if roll == 1 then
        set pick = id1
    elseif roll == 2 then
        set pick = id2
    elseif roll == 3 then
        set pick = id3
    elseif roll == 4 then
        set pick = id4
    else
        // Unrecognized id index - pick the first hero in the list.
        set pick = id1
    endif

    // Create the hero.
    set hero = CreateUnitAtLoc(p, pick, loc, bj_UNIT_FACING)
    if bj_meleeGrantHeroItems then
        call MeleeGrantItemsToHero(hero)
    endif
    return hero
endfunction

//===========================================================================
// Returns a location which is (distance) away from (src) in the direction of (targ).
//

/**
@note It returns a new location, does not remove or alter location passed as `src` or `targ`.
It means you must call `RemoveLocation` yourself after calling this function.

@patch 1.00
*/
function MeleeGetProjectedLoc takes location src, location targ, real distance, real deltaAngle returns location
    local real srcX = GetLocationX(src)
    local real srcY = GetLocationY(src)
    local real direction = Atan2(GetLocationY(targ) - srcY, GetLocationX(targ) - srcX) + deltaAngle
    return Location(srcX + distance * Cos(direction), srcY + distance * Sin(direction))
endfunction

//===========================================================================

/**
It's the typical math.clamp. Returns `val` if it's within the bounds, else min/maxVal.

`minVal` must be less than or equal <= `maxVal`

@patch 1.00
*/
function MeleeGetNearestValueWithin takes real val, real minVal, real maxVal returns real
    if (val < minVal) then
        return minVal
    elseif (val > maxVal) then
        return maxVal
    else
        return val
    endif
endfunction

//===========================================================================

/**
@note It returns a new location, does not remove or alter location passed as `src`.
It means you must call `RemoveLocation` yourself after calling this function.

@patch 1.00
*/
function MeleeGetLocWithinRect takes location src, rect r returns location
    local real withinX = MeleeGetNearestValueWithin(GetLocationX(src), GetRectMinX(r), GetRectMaxX(r))
    local real withinY = MeleeGetNearestValueWithin(GetLocationY(src), GetRectMinY(r), GetRectMaxY(r))
    return Location(withinX, withinY)
endfunction

//===========================================================================
// Starting Units for Human Players
//   - 1 Town Hall, placed at start location
//   - 5 Peasants, placed between start location and nearest gold mine
//

/**
@bug Leaks.

Uncleared handle variables:

1. `nearestMine`
2. `nearMineLoc`
3. `heroLoc`
4. `townHall`

In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

Objects not removed (e.g. RemoveLocation):

1. Loc, Loc `nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
2. Loc, Loc `heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
3. Loc `heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)`

@patch 1.00
*/
function MeleeStartingUnitsHuman takes player whichPlayer, location startLoc, boolean doHeroes, boolean doCamera, boolean doPreload returns nothing
    local boolean  useRandomHero = IsMapFlagSet(MAP_RANDOM_HERO)
    local real     unitSpacing   = 64.00
    local unit     nearestMine
    local location nearMineLoc
    local location heroLoc
    local real     peonX
    local real     peonY
    local unit     townHall = null

    if (doPreload) then
        call Preloader( "scripts\\HumanMelee.pld" )
    endif

    set nearestMine = MeleeFindNearestMine(startLoc, bj_MELEE_MINE_SEARCH_RADIUS)
    if (nearestMine != null) then
        // Spawn Town Hall at the start location.
        set townHall = CreateUnitAtLoc(whichPlayer, 'htow', startLoc, bj_UNIT_FACING)
        
        // Spawn Peasants near the mine.
        set nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 320, 0)
        set peonX = GetLocationX(nearMineLoc)
        set peonY = GetLocationY(nearMineLoc)
        call CreateUnit(whichPlayer, 'hpea', peonX + 0.00 * unitSpacing, peonY + 1.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX + 1.00 * unitSpacing, peonY + 0.15 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX - 1.00 * unitSpacing, peonY + 0.15 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX + 0.60 * unitSpacing, peonY - 1.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX - 0.60 * unitSpacing, peonY - 1.00 * unitSpacing, bj_UNIT_FACING)

        // Set random hero spawn point to be off to the side of the start location.
        set heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 384, 45)
    else
        // Spawn Town Hall at the start location.
        set townHall = CreateUnitAtLoc(whichPlayer, 'htow', startLoc, bj_UNIT_FACING)
        
        // Spawn Peasants directly south of the town hall.
        set peonX = GetLocationX(startLoc)
        set peonY = GetLocationY(startLoc) - 224.00
        call CreateUnit(whichPlayer, 'hpea', peonX + 2.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX + 1.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX + 0.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX - 1.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'hpea', peonX - 2.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)

        // Set random hero spawn point to be just south of the start location.
        set heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)
    endif

    if (townHall != null) then
        call UnitAddAbilityBJ('Amic', townHall)
        call UnitMakeAbilityPermanentBJ(true, 'Amic', townHall)
    endif

    if (doHeroes) then
        // If the "Random Hero" option is set, start the player with a random hero.
        // Otherwise, give them a "free hero" token.
        if useRandomHero then
            call MeleeRandomHeroLoc(whichPlayer, 'Hamg', 'Hmkg', 'Hpal', 'Hblm', heroLoc)
        else
            call SetPlayerState(whichPlayer, PLAYER_STATE_RESOURCE_HERO_TOKENS, bj_MELEE_STARTING_HERO_TOKENS)
        endif
    endif

    if (doCamera) then
        // Center the camera on the initial Peasants.
        call SetCameraPositionForPlayer(whichPlayer, peonX, peonY)
        call SetCameraQuickPositionForPlayer(whichPlayer, peonX, peonY)
    endif
endfunction

//===========================================================================
// Starting Units for Orc Players
//   - 1 Great Hall, placed at start location
//   - 5 Peons, placed between start location and nearest gold mine
//

/**
@bug Leaks.

Uncleared handle variables:

1. `nearestMine`
2. `nearMineLoc`
3. `heroLoc`

In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

Objects not removed (e.g. RemoveLocation):

1. Loc, Loc `nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
2. Loc, Loc `heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
3. Loc `heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)`

@patch 1.00
*/
function MeleeStartingUnitsOrc takes player whichPlayer, location startLoc, boolean doHeroes, boolean doCamera, boolean doPreload returns nothing
    local boolean  useRandomHero = IsMapFlagSet(MAP_RANDOM_HERO)
    local real     unitSpacing   = 64.00
    local unit     nearestMine
    local location nearMineLoc
    local location heroLoc
    local real     peonX
    local real     peonY

    if (doPreload) then
        call Preloader( "scripts\\OrcMelee.pld" )
    endif

    set nearestMine = MeleeFindNearestMine(startLoc, bj_MELEE_MINE_SEARCH_RADIUS)
    if (nearestMine != null) then
        // Spawn Great Hall at the start location.
        call CreateUnitAtLoc(whichPlayer, 'ogre', startLoc, bj_UNIT_FACING)
        
        // Spawn Peons near the mine.
        set nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 320, 0)
        set peonX = GetLocationX(nearMineLoc)
        set peonY = GetLocationY(nearMineLoc)
        call CreateUnit(whichPlayer, 'opeo', peonX + 0.00 * unitSpacing, peonY + 1.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX + 1.00 * unitSpacing, peonY + 0.15 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX - 1.00 * unitSpacing, peonY + 0.15 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX + 0.60 * unitSpacing, peonY - 1.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX - 0.60 * unitSpacing, peonY - 1.00 * unitSpacing, bj_UNIT_FACING)

        // Set random hero spawn point to be off to the side of the start location.
        set heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 384, 45)
    else
        // Spawn Great Hall at the start location.
        call CreateUnitAtLoc(whichPlayer, 'ogre', startLoc, bj_UNIT_FACING)
        
        // Spawn Peons directly south of the town hall.
        set peonX = GetLocationX(startLoc)
        set peonY = GetLocationY(startLoc) - 224.00
        call CreateUnit(whichPlayer, 'opeo', peonX + 2.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX + 1.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX + 0.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX - 1.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'opeo', peonX - 2.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)

        // Set random hero spawn point to be just south of the start location.
        set heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)
    endif

    if (doHeroes) then
        // If the "Random Hero" option is set, start the player with a random hero.
        // Otherwise, give them a "free hero" token.
        if useRandomHero then
            call MeleeRandomHeroLoc(whichPlayer, 'Obla', 'Ofar', 'Otch', 'Oshd', heroLoc)
        else
            call SetPlayerState(whichPlayer, PLAYER_STATE_RESOURCE_HERO_TOKENS, bj_MELEE_STARTING_HERO_TOKENS)
        endif
    endif

    if (doCamera) then
        // Center the camera on the initial Peons.
        call SetCameraPositionForPlayer(whichPlayer, peonX, peonY)
        call SetCameraQuickPositionForPlayer(whichPlayer, peonX, peonY)
    endif
endfunction

//===========================================================================
// Starting Units for Undead Players
//   - 1 Necropolis, placed at start location
//   - 1 Haunted Gold Mine, placed on nearest gold mine
//   - 3 Acolytes, placed between start location and nearest gold mine
//   - 1 Ghoul, placed between start location and nearest gold mine
//   - Blight, centered on nearest gold mine, spread across a "large area"
//

/**
@bug Leaks.

Uncleared handle variables:

1. `nearestMine`
2. `nearMineLoc`
3. `nearTownLoc`
4. `heroLoc`

In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

Objects not removed (e.g. RemoveLocation):

1. Loc, Loc `nearTownLoc = MeleeGetProjectedLoc(startLoc, GetUnitLoc(nearestMine)`
2. Loc, Loc `nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
3. Loc, Loc `heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
4. Loc, Loc `heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
5. Loc `heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)`

@patch 1.00
*/
function MeleeStartingUnitsUndead takes player whichPlayer, location startLoc, boolean doHeroes, boolean doCamera, boolean doPreload returns nothing
    local boolean  useRandomHero = IsMapFlagSet(MAP_RANDOM_HERO)
    local real     unitSpacing   = 64.00
    local unit     nearestMine
    local location nearMineLoc
    local location nearTownLoc
    local location heroLoc
    local real     peonX
    local real     peonY
    local real     ghoulX
    local real     ghoulY

    if (doPreload) then
        call Preloader( "scripts\\UndeadMelee.pld" )
    endif

    set nearestMine = MeleeFindNearestMine(startLoc, bj_MELEE_MINE_SEARCH_RADIUS)
    if (nearestMine != null) then
        // Spawn Necropolis at the start location.
        call CreateUnitAtLoc(whichPlayer, 'unpl', startLoc, bj_UNIT_FACING)
        
        // Replace the nearest gold mine with a blighted version.
        set nearestMine = BlightGoldMineForPlayerBJ(nearestMine, whichPlayer)

        // Spawn Ghoul near the Necropolis.
        set nearTownLoc = MeleeGetProjectedLoc(startLoc, GetUnitLoc(nearestMine), 288, 0)
        set ghoulX = GetLocationX(nearTownLoc)
        set ghoulY = GetLocationY(nearTownLoc)
        set bj_ghoul[GetPlayerId(whichPlayer)] = CreateUnit(whichPlayer, 'ugho', ghoulX + 0.00 * unitSpacing, ghoulY + 0.00 * unitSpacing, bj_UNIT_FACING)

        // Spawn Acolytes near the mine.
        set nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 320, 0)
        set peonX = GetLocationX(nearMineLoc)
        set peonY = GetLocationY(nearMineLoc)
        call CreateUnit(whichPlayer, 'uaco', peonX + 0.00 * unitSpacing, peonY + 0.50 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'uaco', peonX + 0.65 * unitSpacing, peonY - 0.50 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'uaco', peonX - 0.65 * unitSpacing, peonY - 0.50 * unitSpacing, bj_UNIT_FACING)

        // Create a patch of blight around the gold mine.
        call SetBlightLoc(whichPlayer,nearMineLoc, 768, true)

        // Set random hero spawn point to be off to the side of the start location.
        set heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 384, 45)
    else
        // Spawn Necropolis at the start location.
        call CreateUnitAtLoc(whichPlayer, 'unpl', startLoc, bj_UNIT_FACING)
        
        // Spawn Acolytes and Ghoul directly south of the Necropolis.
        set peonX = GetLocationX(startLoc)
        set peonY = GetLocationY(startLoc) - 224.00
        call CreateUnit(whichPlayer, 'uaco', peonX - 1.50 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'uaco', peonX - 0.50 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'uaco', peonX + 0.50 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ugho', peonX + 1.50 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)

        // Create a patch of blight around the start location.
        call SetBlightLoc(whichPlayer,startLoc, 768, true)

        // Set random hero spawn point to be just south of the start location.
        set heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)
    endif

    if (doHeroes) then
        // If the "Random Hero" option is set, start the player with a random hero.
        // Otherwise, give them a "free hero" token.
        if useRandomHero then
            call MeleeRandomHeroLoc(whichPlayer, 'Udea', 'Udre', 'Ulic', 'Ucrl', heroLoc)
        else
            call SetPlayerState(whichPlayer, PLAYER_STATE_RESOURCE_HERO_TOKENS, bj_MELEE_STARTING_HERO_TOKENS)
        endif
    endif

    if (doCamera) then
        // Center the camera on the initial Acolytes.
        call SetCameraPositionForPlayer(whichPlayer, peonX, peonY)
        call SetCameraQuickPositionForPlayer(whichPlayer, peonX, peonY)
    endif
endfunction

//===========================================================================
// Starting Units for Night Elf Players
//   - 1 Tree of Life, placed by nearest gold mine, already entangled
//   - 5 Wisps, placed between Tree of Life and nearest gold mine
//

/**
@bug Leaks.

Uncleared handle variables:

1. `nearestMine`
2. `nearMineLoc`
3. `wispLoc`
4. `heroLoc`
5. `tree`

In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

Objects not removed (e.g. RemoveLocation):

1. Location,Location `nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
2. Location, Rect, Location `nearMineLoc = MeleeGetLocWithinRect(nearMineLoc, GetRectFromCircleBJ(GetUnitLoc(nearestMine), minTreeDist))`
3. Loc, Loc `wispLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 320, 0)`
4. Loc, Rect, Loc `wispLoc = MeleeGetLocWithinRect(wispLoc, GetRectFromCircleBJ(GetUnitLoc(nearestMine`
5. Loc, Loc `heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine)`
6. Loc `heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)`

@patch 1.00
*/
function MeleeStartingUnitsNightElf takes player whichPlayer, location startLoc, boolean doHeroes, boolean doCamera, boolean doPreload returns nothing
    local boolean  useRandomHero = IsMapFlagSet(MAP_RANDOM_HERO)
    local real     unitSpacing   = 64.00
    local real     minTreeDist   = 3.50 * bj_CELLWIDTH
    local real     minWispDist   = 1.75 * bj_CELLWIDTH
    local unit     nearestMine
    local location nearMineLoc
    local location wispLoc
    local location heroLoc
    local real     peonX
    local real     peonY
    local unit     tree

    if (doPreload) then
        call Preloader( "scripts\\NightElfMelee.pld" )
    endif

    set nearestMine = MeleeFindNearestMine(startLoc, bj_MELEE_MINE_SEARCH_RADIUS)
    if (nearestMine != null) then
        // Spawn Tree of Life near the mine and have it entangle the mine.
        // Project the Tree's coordinates from the gold mine, and then snap
        // the X and Y values to within minTreeDist of the Gold Mine.
        set nearMineLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 650, 0)
        set nearMineLoc = MeleeGetLocWithinRect(nearMineLoc, GetRectFromCircleBJ(GetUnitLoc(nearestMine), minTreeDist))
        set tree = CreateUnitAtLoc(whichPlayer, 'etol', nearMineLoc, bj_UNIT_FACING)
        call IssueTargetOrder(tree, "entangleinstant", nearestMine)

        // Spawn Wisps at the start location.
        set wispLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 320, 0)
        set wispLoc = MeleeGetLocWithinRect(wispLoc, GetRectFromCircleBJ(GetUnitLoc(nearestMine), minWispDist))
        set peonX = GetLocationX(wispLoc)
        set peonY = GetLocationY(wispLoc)
        call CreateUnit(whichPlayer, 'ewsp', peonX + 0.00 * unitSpacing, peonY + 1.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX + 1.00 * unitSpacing, peonY + 0.15 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX - 1.00 * unitSpacing, peonY + 0.15 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX + 0.58 * unitSpacing, peonY - 1.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX - 0.58 * unitSpacing, peonY - 1.00 * unitSpacing, bj_UNIT_FACING)

        // Set random hero spawn point to be off to the side of the start location.
        set heroLoc = MeleeGetProjectedLoc(GetUnitLoc(nearestMine), startLoc, 384, 45)
    else
        // Spawn Tree of Life at the start location.
        call CreateUnitAtLoc(whichPlayer, 'etol', startLoc, bj_UNIT_FACING)

        // Spawn Wisps directly south of the town hall.
        set peonX = GetLocationX(startLoc)
        set peonY = GetLocationY(startLoc) - 224.00
        call CreateUnit(whichPlayer, 'ewsp', peonX - 2.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX - 1.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX + 0.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX + 1.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)
        call CreateUnit(whichPlayer, 'ewsp', peonX + 2.00 * unitSpacing, peonY + 0.00 * unitSpacing, bj_UNIT_FACING)

        // Set random hero spawn point to be just south of the start location.
        set heroLoc = Location(peonX, peonY - 2.00 * unitSpacing)
    endif

    if (doHeroes) then
        // If the "Random Hero" option is set, start the player with a random hero.
        // Otherwise, give them a "free hero" token.
        if useRandomHero then
            call MeleeRandomHeroLoc(whichPlayer, 'Edem', 'Ekee', 'Emoo', 'Ewar', heroLoc)
        else
            call SetPlayerState(whichPlayer, PLAYER_STATE_RESOURCE_HERO_TOKENS, bj_MELEE_STARTING_HERO_TOKENS)
        endif
    endif

    if (doCamera) then
        // Center the camera on the initial Wisps.
        call SetCameraPositionForPlayer(whichPlayer, peonX, peonY)
        call SetCameraQuickPositionForPlayer(whichPlayer, peonX, peonY)
    endif
endfunction

//===========================================================================
// Starting Units for Players Whose Race is Unknown
//   - 12 Sheep, placed randomly around the start location
//

/**
@patch 1.00
*/
function MeleeStartingUnitsUnknownRace takes player whichPlayer, location startLoc, boolean doHeroes, boolean doCamera, boolean doPreload returns nothing
    local integer index

    if (doPreload) then
    endif

    set index = 0
    loop
        call CreateUnit(whichPlayer, 'nshe', GetLocationX(startLoc) + GetRandomReal(-256, 256), GetLocationY(startLoc) + GetRandomReal(-256, 256), GetRandomReal(0, 360))
        set index = index + 1
        exitwhen index == 12
    endloop

    if (doHeroes) then
        // Give them a "free hero" token, out of pity.
        call SetPlayerState(whichPlayer, PLAYER_STATE_RESOURCE_HERO_TOKENS, bj_MELEE_STARTING_HERO_TOKENS)
    endif

    if (doCamera) then
        // Center the camera on the initial sheep.
        call SetCameraPositionLocForPlayer(whichPlayer, startLoc)
        call SetCameraQuickPositionLocForPlayer(whichPlayer, startLoc)
    endif
endfunction

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@bug Leaks handle `indexStartLoc`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeStartingUnits takes nothing returns nothing
    local integer  index
    local player   indexPlayer
    local location indexStartLoc
    local race     indexRace

    call Preloader( "scripts\\SharedMelee.pld" )

    set index = 0
    loop
        set indexPlayer = Player(index)
        if (GetPlayerSlotState(indexPlayer) == PLAYER_SLOT_STATE_PLAYING) then
            set indexStartLoc = GetStartLocationLoc(GetPlayerStartLocation(indexPlayer))
            set indexRace = GetPlayerRace(indexPlayer)

            // Create initial race-specific starting units
            if (indexRace == RACE_HUMAN) then
                call MeleeStartingUnitsHuman(indexPlayer, indexStartLoc, true, true, true)
            elseif (indexRace == RACE_ORC) then
                call MeleeStartingUnitsOrc(indexPlayer, indexStartLoc, true, true, true)
            elseif (indexRace == RACE_UNDEAD) then
                call MeleeStartingUnitsUndead(indexPlayer, indexStartLoc, true, true, true)
            elseif (indexRace == RACE_NIGHTELF) then
                call MeleeStartingUnitsNightElf(indexPlayer, indexStartLoc, true, true, true)
            else
                call MeleeStartingUnitsUnknownRace(indexPlayer, indexStartLoc, true, true, true)
            endif
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
    
endfunction

//===========================================================================

/**
@patch 1.07
*/
function MeleeStartingUnitsForPlayer takes race whichRace, player whichPlayer, location loc, boolean doHeroes returns nothing
    // Create initial race-specific starting units
    if (whichRace == RACE_HUMAN) then
        call MeleeStartingUnitsHuman(whichPlayer, loc, doHeroes, false, false)
    elseif (whichRace == RACE_ORC) then
        call MeleeStartingUnitsOrc(whichPlayer, loc, doHeroes, false, false)
    elseif (whichRace == RACE_UNDEAD) then
        call MeleeStartingUnitsUndead(whichPlayer, loc, doHeroes, false, false)
    elseif (whichRace == RACE_NIGHTELF) then
        call MeleeStartingUnitsNightElf(whichPlayer, loc, doHeroes, false, false)
    else
        // Unrecognized race - ignore the request.
    endif
endfunction



//***************************************************************************
//*
//*  Melee Template Starting AI Scripts
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.07
*/
function PickMeleeAI takes player num, string s1, string s2, string s3 returns nothing
    local integer pick

    // easy difficulty never uses any custom AI scripts
    // that are designed to be a bit more challenging
    //
    if GetAIDifficulty(num) == AI_DIFFICULTY_NEWBIE then
        call StartMeleeAI(num,s1)
        return
    endif

    if s2 == null then
        set pick = 1
    elseif s3 == null then
        set pick = GetRandomInt(1,2)
    else
        set pick = GetRandomInt(1,3)
    endif

    if pick == 1 then
        call StartMeleeAI(num,s1)
    elseif pick == 2 then
        call StartMeleeAI(num,s2)
    else
        call StartMeleeAI(num,s3)
    endif
endfunction

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@patch 1.00
*/
function MeleeStartingAI takes nothing returns nothing
    local integer index
    local player  indexPlayer
    local race    indexRace

    set index = 0
    loop
        set indexPlayer = Player(index)
        if (GetPlayerSlotState(indexPlayer) == PLAYER_SLOT_STATE_PLAYING) then
            set indexRace = GetPlayerRace(indexPlayer)
            if (GetPlayerController(indexPlayer) == MAP_CONTROL_COMPUTER) then
                // Run a race-specific melee AI script.
                if (indexRace == RACE_HUMAN) then
                    call PickMeleeAI(indexPlayer, "human.ai", null, null)
                elseif (indexRace == RACE_ORC) then
                    call PickMeleeAI(indexPlayer, "orc.ai", null, null)
                elseif (indexRace == RACE_UNDEAD) then
                    call PickMeleeAI(indexPlayer, "undead.ai", null, null)
                    call RecycleGuardPosition(bj_ghoul[index])
                elseif (indexRace == RACE_NIGHTELF) then
                    call PickMeleeAI(indexPlayer, "elf.ai", null, null)
                else
                    // Unrecognized race.
                endif
                call ShareEverythingWithTeamAI(indexPlayer)
            endif
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction


/**
@patch 1.07
*/
function LockGuardPosition takes unit targ returns nothing
    call SetUnitCreepGuard(targ,true)
endfunction


//***************************************************************************
//*
//*  Melee Template Victory / Defeat Conditions
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function MeleePlayerIsOpponent takes integer playerIndex, integer opponentIndex returns boolean
    local player thePlayer = Player(playerIndex)
    local player theOpponent = Player(opponentIndex)

    // The player himself is not an opponent.
    if (playerIndex == opponentIndex) then
        return false
    endif

    // Unused player slots are not opponents.
    if (GetPlayerSlotState(theOpponent) != PLAYER_SLOT_STATE_PLAYING) then
        return false
    endif

    // Players who are already defeated are not opponents.
    if (bj_meleeDefeated[opponentIndex]) then
        return false
    endif

    // Allied players with allied victory set are not opponents.
    if GetPlayerAlliance(thePlayer, theOpponent, ALLIANCE_PASSIVE) then
        if GetPlayerAlliance(theOpponent, thePlayer, ALLIANCE_PASSIVE) then
            if (GetPlayerState(thePlayer, PLAYER_STATE_ALLIED_VICTORY) == 1) then
                if (GetPlayerState(theOpponent, PLAYER_STATE_ALLIED_VICTORY) == 1) then
                    return false
                endif
            endif
        endif
    endif

    return true
endfunction

//===========================================================================
// Count buildings currently owned by all allies, including the player themself.
//

/**
@patch 1.00
*/
function MeleeGetAllyStructureCount takes player whichPlayer returns integer
    local integer    playerIndex
    local integer    buildingCount
    local player     indexPlayer

    // Count the number of buildings controlled by all not-yet-defeated co-allies.
    set buildingCount = 0
    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)

        // uncomment to cause defeat even if you have control of ally structures, but yours have been nixed
        //if (PlayersAreCoAllied(whichPlayer, indexPlayer) and not bj_meleeDefeated[playerIndex]) then
        if (PlayersAreCoAllied(whichPlayer, indexPlayer)) then
            set buildingCount = buildingCount + GetPlayerStructureCount(indexPlayer, true)
        endif
            
        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop

    return buildingCount
endfunction

//===========================================================================
// Count allies, excluding dead players and the player themself.
//

/**
@patch 1.00
*/
function MeleeGetAllyCount takes player whichPlayer returns integer
    local integer playerIndex
    local integer playerCount
    local player  indexPlayer

    // Count the number of not-yet-defeated co-allies.
    set playerCount = 0
    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        if PlayersAreCoAllied(whichPlayer, indexPlayer) and not bj_meleeDefeated[playerIndex] and (whichPlayer != indexPlayer) then
            set playerCount = playerCount + 1
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop

    return playerCount
endfunction

//===========================================================================
// Counts key structures owned by a player and his or her allies, including
// structures currently upgrading or under construction.
//
// Key structures: Town Hall, Great Hall, Tree of Life, Necropolis
//

/**
@patch 1.07
*/
function MeleeGetAllyKeyStructureCount takes player whichPlayer returns integer
    local integer    playerIndex
    local player     indexPlayer
    local integer    keyStructs

    // Count the number of buildings controlled by all not-yet-defeated co-allies.
    set keyStructs = 0
    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        if (PlayersAreCoAllied(whichPlayer, indexPlayer)) then
            set keyStructs = keyStructs + BlzGetPlayerTownHallCount(indexPlayer)
        endif
            
        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop

    return keyStructs
endfunction

//===========================================================================
// Enum: Draw out a specific player.
//

/**
@patch 1.07
*/
function MeleeDoDrawEnum takes nothing returns nothing
    local player thePlayer = GetEnumPlayer()

    call CachePlayerHeroData(thePlayer)
    call RemovePlayerPreserveUnitsBJ(thePlayer, PLAYER_GAME_RESULT_TIE, false)
endfunction

//===========================================================================
// Enum: Victory out a specific player.
//

/**
@patch 1.00
*/
function MeleeDoVictoryEnum takes nothing returns nothing
    local player thePlayer = GetEnumPlayer()
    local integer playerIndex = GetPlayerId(thePlayer)

    if (not bj_meleeVictoried[playerIndex]) then
        set bj_meleeVictoried[playerIndex] = true
        call CachePlayerHeroData(thePlayer)
        call RemovePlayerPreserveUnitsBJ(thePlayer, PLAYER_GAME_RESULT_VICTORY, false)
    endif
endfunction

//===========================================================================
// Defeat out a specific player.
//

/**
@patch 1.00
*/
function MeleeDoDefeat takes player whichPlayer returns nothing
    set bj_meleeDefeated[GetPlayerId(whichPlayer)] = true
    call RemovePlayerPreserveUnitsBJ(whichPlayer, PLAYER_GAME_RESULT_DEFEAT, false)
endfunction

//===========================================================================
// Enum: Defeat out a specific player.
//

/**
@patch 1.00
*/
function MeleeDoDefeatEnum takes nothing returns nothing
    local player thePlayer = GetEnumPlayer()

    // needs to happen before ownership change
    call CachePlayerHeroData(thePlayer)
    call MakeUnitsPassiveForTeam(thePlayer)
    call MeleeDoDefeat(thePlayer)
endfunction

//===========================================================================
// A specific player left the game.
//

/**
@patch 1.00
*/
function MeleeDoLeave takes player whichPlayer returns nothing
    if (GetIntegerGameState(GAME_STATE_DISCONNECTED) != 0) then
        call GameOverDialogBJ( whichPlayer, true )
    else
        set bj_meleeDefeated[GetPlayerId(whichPlayer)] = true
        call RemovePlayerPreserveUnitsBJ(whichPlayer, PLAYER_GAME_RESULT_DEFEAT, true)
    endif
endfunction

//===========================================================================
// Remove all observers
// 

/**
@patch 1.00
*/
function MeleeRemoveObservers takes nothing returns nothing
    local integer    playerIndex
    local player     indexPlayer

    // Give all observers the game over dialog
    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)

        if (IsPlayerObserver(indexPlayer)) then
            call RemovePlayerPreserveUnitsBJ(indexPlayer, PLAYER_GAME_RESULT_NEUTRAL, false)
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================
// Test all players to determine if a team has won.  For a team to win, all
// remaining (read: undefeated) players need to be co-allied with all other
// remaining players.  If even one player is not allied towards another,
// everyone must be denied victory.
//

/**
@bug Leaks handle `opponentlessPlayers`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeCheckForVictors takes nothing returns force
    local integer    playerIndex
    local integer    opponentIndex
    local force      opponentlessPlayers = CreateForce()
    local boolean    gameOver = false

    // Check to see if any players have opponents remaining.
    set playerIndex = 0
    loop
        if (not bj_meleeDefeated[playerIndex]) then
            // Determine whether or not this player has any remaining opponents.
            set opponentIndex = 0
            loop
                // If anyone has an opponent, noone can be victorious yet.
                if MeleePlayerIsOpponent(playerIndex, opponentIndex) then
                    return CreateForce()
                endif

                set opponentIndex = opponentIndex + 1
                exitwhen opponentIndex == bj_MAX_PLAYERS
            endloop

            // Keep track of each opponentless player so that we can give
            // them a victory later.
            call ForceAddPlayer(opponentlessPlayers, Player(playerIndex))
            set gameOver = true
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop

    // Set the game over global flag
    set bj_meleeGameOver = gameOver

    return opponentlessPlayers
endfunction

//===========================================================================
// Test each player to determine if anyone has been defeated.
//

/**
@bug Leaks handle `defeatedPlayers`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@bug Leaks handle `victoriousPlayers`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function MeleeCheckForLosersAndVictors takes nothing returns nothing
    local integer    playerIndex
    local player     indexPlayer
    local force      defeatedPlayers = CreateForce()
    local force      victoriousPlayers
    local boolean    gameOver = false

    // If the game is already over, do nothing
    if (bj_meleeGameOver) then
        return
    endif

    // If the game was disconnected then it is over, in this case we
    // don't want to report results for anyone as they will most likely
    // conflict with the actual game results
    if (GetIntegerGameState(GAME_STATE_DISCONNECTED) != 0) then
        set bj_meleeGameOver = true
        return
    endif

    // Check each player to see if he or she has been defeated yet.
    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)

        if (not bj_meleeDefeated[playerIndex] and not bj_meleeVictoried[playerIndex]) then
            //call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, 60, "Player"+I2S(playerIndex)+" has "+I2S(MeleeGetAllyStructureCount(indexPlayer))+" ally buildings.")
            if (MeleeGetAllyStructureCount(indexPlayer) <= 0) then

                // Keep track of each defeated player so that we can give
                // them a defeat later.
                call ForceAddPlayer(defeatedPlayers, Player(playerIndex))

                // Set their defeated flag now so MeleeCheckForVictors
                // can detect victors.
                set bj_meleeDefeated[playerIndex] = true
            endif
        endif
            
        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop

    // Now that the defeated flags are set, check if there are any victors
    set victoriousPlayers = MeleeCheckForVictors()

    // Defeat all defeated players
    call ForForce(defeatedPlayers, function MeleeDoDefeatEnum)

    // Give victory to all victorious players
    call ForForce(victoriousPlayers, function MeleeDoVictoryEnum)

    // If the game is over we should remove all observers
    if (bj_meleeGameOver) then
        call MeleeRemoveObservers()
    endif
endfunction

//===========================================================================
// Returns a race-specific "build X or be revealed" message.
//

/**
@patch 1.07
*/
function MeleeGetCrippledWarningMessage takes player whichPlayer returns string
    local race r = GetPlayerRace(whichPlayer)

    if (r == RACE_HUMAN) then
        return GetLocalizedString("CRIPPLE_WARNING_HUMAN")
    elseif (r == RACE_ORC) then
        return GetLocalizedString("CRIPPLE_WARNING_ORC")
    elseif (r == RACE_NIGHTELF) then
        return GetLocalizedString("CRIPPLE_WARNING_NIGHTELF")
    elseif (r == RACE_UNDEAD) then
        return GetLocalizedString("CRIPPLE_WARNING_UNDEAD")
    else
        // Unrecognized Race
        return ""
    endif
endfunction

//===========================================================================
// Returns a race-specific "build X" label for cripple timers.
//

/**
@patch 1.07
*/
function MeleeGetCrippledTimerMessage takes player whichPlayer returns string
    local race r = GetPlayerRace(whichPlayer)

    if (r == RACE_HUMAN) then
        return GetLocalizedString("CRIPPLE_TIMER_HUMAN")
    elseif (r == RACE_ORC) then
        return GetLocalizedString("CRIPPLE_TIMER_ORC")
    elseif (r == RACE_NIGHTELF) then
        return GetLocalizedString("CRIPPLE_TIMER_NIGHTELF")
    elseif (r == RACE_UNDEAD) then
        return GetLocalizedString("CRIPPLE_TIMER_UNDEAD")
    else
        // Unrecognized Race
        return ""
    endif
endfunction

//===========================================================================
// Returns a race-specific "build X" label for cripple timers.
//

/**
@patch 1.07
*/
function MeleeGetCrippledRevealedMessage takes player whichPlayer returns string
    return GetLocalizedString("CRIPPLE_REVEALING_PREFIX") + GetPlayerName(whichPlayer) + GetLocalizedString("CRIPPLE_REVEALING_POSTFIX")
endfunction

//===========================================================================

/**
@bug Leaks handle `toExposeTo`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function MeleeExposePlayer takes player whichPlayer, boolean expose returns nothing
    local integer playerIndex
    local player  indexPlayer
    local force   toExposeTo = CreateForce()

    call CripplePlayer( whichPlayer, toExposeTo, false )

    set bj_playerIsExposed[GetPlayerId(whichPlayer)] = expose
    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        if (not PlayersAreCoAllied(whichPlayer, indexPlayer)) then
            call ForceAddPlayer( toExposeTo, indexPlayer )
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop

    call CripplePlayer( whichPlayer, toExposeTo, expose )
    call DestroyForce(toExposeTo)
endfunction

//===========================================================================

/**
@bug Leaks handle `toExposeTo`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function MeleeExposeAllPlayers takes nothing returns nothing
    local integer playerIndex
    local player  indexPlayer
    local integer playerIndex2
    local player  indexPlayer2
    local force   toExposeTo = CreateForce()

    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)

        call ForceClear( toExposeTo )
        call CripplePlayer( indexPlayer, toExposeTo, false )

        set playerIndex2 = 0
        loop
            set indexPlayer2 = Player(playerIndex2)

            if playerIndex != playerIndex2 then
                if (not PlayersAreCoAllied(indexPlayer, indexPlayer2)) then
                    call ForceAddPlayer( toExposeTo, indexPlayer2 )
                endif
            endif

            set playerIndex2 = playerIndex2 + 1
            exitwhen playerIndex2 == bj_MAX_PLAYERS
        endloop

        call CripplePlayer( indexPlayer, toExposeTo, true )

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop

    call DestroyForce( toExposeTo )
endfunction

//===========================================================================

/**
@bug Leaks handle `expiredTimer`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function MeleeCrippledPlayerTimeout takes nothing returns nothing
    local timer expiredTimer = GetExpiredTimer()
    local integer playerIndex
    local player  exposedPlayer

    // Determine which player's timer expired.
    set playerIndex = 0
    loop
        if (bj_crippledTimer[playerIndex] == expiredTimer) then
            exitwhen true
        endif

        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop
    if (playerIndex == bj_MAX_PLAYERS) then
        return
    endif
    set exposedPlayer = Player(playerIndex)

    if (GetLocalPlayer() == exposedPlayer) then
        // Use only local code (no net traffic) within this block to avoid desyncs.

        // Hide the timer window for this player.
        call TimerDialogDisplay(bj_crippledTimerWindows[playerIndex], false)
    endif

    // Display a text message to all players, explaining the exposure.
    call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, bj_MELEE_CRIPPLE_MSG_DURATION, MeleeGetCrippledRevealedMessage(exposedPlayer))

    // Expose the player.
    call MeleeExposePlayer(exposedPlayer, true)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function MeleePlayerIsCrippled takes player whichPlayer returns boolean
    local integer playerStructures  = GetPlayerStructureCount(whichPlayer, true)
    local integer playerKeyStructures = BlzGetPlayerTownHallCount(whichPlayer)

    // Dead players are not considered to be crippled.
    return (playerStructures > 0) and (playerKeyStructures <= 0)
endfunction

//===========================================================================
// Test each player to determine if anyone has become crippled.
//

/**
@bug Leaks handle `crippledPlayers`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

The function was refactored, it no longer uses locals `indexRace` and `crippledPlayers`.

@patch 1.07
*/
function MeleeCheckForCrippledPlayers takes nothing returns nothing
    local integer    playerIndex
    local player     indexPlayer
    local force      crippledPlayers = CreateForce()
    local boolean    isNowCrippled
    local race       indexRace

    // The "finish soon" exposure of all players overrides any "crippled" exposure
    if bj_finishSoonAllExposed then
        return
    endif

    // Check each player to see if he or she has been crippled or uncrippled.
    set playerIndex = 0
    loop
        set indexPlayer = Player(playerIndex)
        set isNowCrippled = MeleePlayerIsCrippled(indexPlayer)

        if (not bj_playerIsCrippled[playerIndex] and isNowCrippled) then

            // Player became crippled; start their cripple timer.
            set bj_playerIsCrippled[playerIndex] = true
            call TimerStart(bj_crippledTimer[playerIndex], bj_MELEE_CRIPPLE_TIMEOUT, false, function MeleeCrippledPlayerTimeout)

            if (GetLocalPlayer() == indexPlayer) then
                // Use only local code (no net traffic) within this block to avoid desyncs.

                // Show the timer window.
                call TimerDialogDisplay(bj_crippledTimerWindows[playerIndex], true)

                // Display a warning message.
                call DisplayTimedTextToPlayer(indexPlayer, 0, 0, bj_MELEE_CRIPPLE_MSG_DURATION, MeleeGetCrippledWarningMessage(indexPlayer))
            endif

        elseif (bj_playerIsCrippled[playerIndex] and not isNowCrippled) then

            // Player became uncrippled; stop their cripple timer.
            set bj_playerIsCrippled[playerIndex] = false
            call PauseTimer(bj_crippledTimer[playerIndex])

            if (GetLocalPlayer() == indexPlayer) then
                // Use only local code (no net traffic) within this block to avoid desyncs.

                // Hide the timer window for this player.
                call TimerDialogDisplay(bj_crippledTimerWindows[playerIndex], false)

                // Display a confirmation message if the player's team is still alive.
                if (MeleeGetAllyStructureCount(indexPlayer) > 0) then
                    if (bj_playerIsExposed[playerIndex]) then
                        call DisplayTimedTextToPlayer(indexPlayer, 0, 0, bj_MELEE_CRIPPLE_MSG_DURATION, GetLocalizedString("CRIPPLE_UNREVEALED"))
                    else
                        call DisplayTimedTextToPlayer(indexPlayer, 0, 0, bj_MELEE_CRIPPLE_MSG_DURATION, GetLocalizedString("CRIPPLE_UNCRIPPLED"))
                    endif
                endif
            endif

            // If the player granted shared vision, deny that vision now.
            call MeleeExposePlayer(indexPlayer, false)

        endif
            
        set playerIndex = playerIndex + 1
        exitwhen playerIndex == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================
// Determine if the lost unit should result in any defeats or victories.
//

/**
@patch 1.00
*/
function MeleeCheckLostUnit takes unit lostUnit returns nothing
    local player lostUnitOwner = GetOwningPlayer(lostUnit)

    // We only need to check for mortality if this was the last building.
    if (GetPlayerStructureCount(lostUnitOwner, true) <= 0) then
        call MeleeCheckForLosersAndVictors()
    endif

    // Check if the lost unit has crippled or uncrippled the player.
    // (A team with 0 units is dead, and thus considered uncrippled.)
    call MeleeCheckForCrippledPlayers()
endfunction

//===========================================================================
// Determine if the gained unit should result in any defeats, victories,
// or cripple-status changes.
//

/**
@patch 1.07
*/
function MeleeCheckAddedUnit takes unit addedUnit returns nothing
    local player addedUnitOwner = GetOwningPlayer(addedUnit)

    // If the player was crippled, this unit may have uncrippled him/her.
    if (bj_playerIsCrippled[GetPlayerId(addedUnitOwner)]) then
        call MeleeCheckForCrippledPlayers()
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MeleeTriggerActionConstructCancel takes nothing returns nothing
    call MeleeCheckLostUnit(GetCancelledStructure())
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MeleeTriggerActionUnitDeath takes nothing returns nothing
    if (IsUnitType(GetDyingUnit(), UNIT_TYPE_STRUCTURE)) then
        call MeleeCheckLostUnit(GetDyingUnit())
    endif
endfunction

//===========================================================================

/**
@patch 1.07
*/
function MeleeTriggerActionUnitConstructionStart takes nothing returns nothing
    call MeleeCheckAddedUnit(GetConstructingStructure())
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MeleeTriggerActionPlayerDefeated takes nothing returns nothing
    local player thePlayer = GetTriggerPlayer()
    call CachePlayerHeroData(thePlayer)

    if (MeleeGetAllyCount(thePlayer) > 0) then
        // If at least one ally is still alive and kicking, share units with
        // them and proceed with death.
        call ShareEverythingWithTeam(thePlayer)
        if (not bj_meleeDefeated[GetPlayerId(thePlayer)]) then
            call MeleeDoDefeat(thePlayer)
        endif
    else
        // If no living allies remain, swap all units and buildings over to
        // neutral_passive and proceed with death.
        call MakeUnitsPassiveForTeam(thePlayer)
        if (not bj_meleeDefeated[GetPlayerId(thePlayer)]) then
            call MeleeDoDefeat(thePlayer)
        endif
    endif
    call MeleeCheckForLosersAndVictors()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MeleeTriggerActionPlayerLeft takes nothing returns nothing
    local player thePlayer = GetTriggerPlayer()

    // Just show game over for observers when they leave
    if (IsPlayerObserver(thePlayer)) then
        call RemovePlayerPreserveUnitsBJ(thePlayer, PLAYER_GAME_RESULT_NEUTRAL, false)
        return
    endif

    call CachePlayerHeroData(thePlayer)

    // This is the same as defeat except the player generates the message 
    // "player left the game" as opposed to "player was defeated".

    if (MeleeGetAllyCount(thePlayer) > 0) then
        // If at least one ally is still alive and kicking, share units with
        // them and proceed with death.
        call ShareEverythingWithTeam(thePlayer)
        call MeleeDoLeave(thePlayer)
    else
        // If no living allies remain, swap all units and buildings over to
        // neutral_passive and proceed with death.
        call MakeUnitsPassiveForTeam(thePlayer)
        call MeleeDoLeave(thePlayer)
    endif
    call MeleeCheckForLosersAndVictors()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MeleeTriggerActionAllianceChange takes nothing returns nothing
    call MeleeCheckForLosersAndVictors()
    call MeleeCheckForCrippledPlayers()
endfunction

//===========================================================================

/**
@patch 1.07
*/
function MeleeTriggerTournamentFinishSoon takes nothing returns nothing
    // Note: We may get this trigger multiple times
    local integer    playerIndex
    local player     indexPlayer
    local real       timeRemaining = GetTournamentFinishSoonTimeRemaining()

    if not bj_finishSoonAllExposed then
        set bj_finishSoonAllExposed = true

        // Reset all crippled players and their timers, and hide the local crippled timer dialog
        set playerIndex = 0
        loop
            set indexPlayer = Player(playerIndex)
            if bj_playerIsCrippled[playerIndex] then
                // Uncripple the player
                set bj_playerIsCrippled[playerIndex] = false
                call PauseTimer(bj_crippledTimer[playerIndex])

                if (GetLocalPlayer() == indexPlayer) then
                    // Use only local code (no net traffic) within this block to avoid desyncs.

                    // Hide the timer window.
                    call TimerDialogDisplay(bj_crippledTimerWindows[playerIndex], false)
                endif

            endif
            set playerIndex = playerIndex + 1
            exitwhen playerIndex == bj_MAX_PLAYERS
        endloop

        // Expose all players
        call MeleeExposeAllPlayers()
    endif

    // Show the "finish soon" timer dialog and set the real time remaining
    call TimerDialogDisplay(bj_finishSoonTimerDialog, true)
    call TimerDialogSetRealTimeRemaining(bj_finishSoonTimerDialog, timeRemaining)
endfunction


//===========================================================================

/**
@patch 1.07
*/
function MeleeWasUserPlayer takes player whichPlayer returns boolean
    local playerslotstate slotState

    if (GetPlayerController(whichPlayer) != MAP_CONTROL_USER) then
        return false
    endif

    set slotState = GetPlayerSlotState(whichPlayer)

    return (slotState == PLAYER_SLOT_STATE_PLAYING or slotState == PLAYER_SLOT_STATE_LEFT)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function MeleeTournamentFinishNowRuleA takes integer multiplier returns nothing
    local integer array playerScore
    local integer array teamScore
    local force array   teamForce
    local integer       teamCount
    local integer       index
    local player        indexPlayer
    local integer       index2
    local player        indexPlayer2
    local integer       bestTeam
    local integer       bestScore
    local boolean       draw

    // Compute individual player scores
    set index = 0
    loop
        set indexPlayer = Player(index)
        if MeleeWasUserPlayer(indexPlayer) then
            set playerScore[index] = GetTournamentScore(indexPlayer)
            if playerScore[index] <= 0 then
                set playerScore[index] = 1
            endif
        else
            set playerScore[index] = 0
        endif
        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop

    // Compute team scores and team forces
    set teamCount = 0
    set index = 0
    loop
        if playerScore[index] != 0 then
            set indexPlayer = Player(index)

            set teamScore[teamCount] = 0
            set teamForce[teamCount] = CreateForce()

            set index2 = index
            loop
                if playerScore[index2] != 0 then
                    set indexPlayer2 = Player(index2)

                    if PlayersAreCoAllied(indexPlayer, indexPlayer2) then
                        set teamScore[teamCount] = teamScore[teamCount] + playerScore[index2]
                        call ForceAddPlayer(teamForce[teamCount], indexPlayer2)
                        set playerScore[index2] = 0
                    endif
                endif

                set index2 = index2 + 1
                exitwhen index2 == bj_MAX_PLAYERS
            endloop

            set teamCount = teamCount + 1
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop

    // The game is now over
    set bj_meleeGameOver = true

    // There should always be at least one team, but continue to work if not
    if teamCount != 0 then

        // Find best team score
        set bestTeam = -1
        set bestScore = -1
        set index = 0
        loop
            if teamScore[index] > bestScore then
                set bestTeam = index
                set bestScore = teamScore[index]
            endif

            set index = index + 1
            exitwhen index == teamCount
        endloop

        // Check whether the best team's score is 'multiplier' times better than
        // every other team. In the case of multiplier == 1 and exactly equal team
        // scores, the first team (which was randomly chosen by the server) will win.
        set draw = false
        set index = 0
        loop
            if index != bestTeam then
                if bestScore < (multiplier * teamScore[index]) then
                    set draw = true
                endif
            endif

            set index = index + 1
            exitwhen index == teamCount
        endloop

        if draw then
            // Give draw to all players on all teams
            set index = 0
            loop
                call ForForce(teamForce[index], function MeleeDoDrawEnum)

                set index = index + 1
                exitwhen index == teamCount
            endloop
        else
            // Give defeat to all players on teams other than the best team
            set index = 0
            loop
                if index != bestTeam then
                    call ForForce(teamForce[index], function MeleeDoDefeatEnum)
                endif

                set index = index + 1
                exitwhen index == teamCount
            endloop

            // Give victory to all players on the best team
            call ForForce(teamForce[bestTeam], function MeleeDoVictoryEnum)
        endif
    endif

endfunction

//===========================================================================

/**
@patch 1.07
*/
function MeleeTriggerTournamentFinishNow takes nothing returns nothing
    local integer rule = GetTournamentFinishNowRule()

    // If the game is already over, do nothing
    if bj_meleeGameOver then
        return
    endif

    if (rule == 1) then
        // Finals games
        call MeleeTournamentFinishNowRuleA(1)
    else
        // Preliminary games
        call MeleeTournamentFinishNowRuleA(3)
    endif

    // Since the game is over we should remove all observers
    call MeleeRemoveObservers()

endfunction

//===========================================================================

/**
@note It is called directly by the default "Melee Initialization" trigger.

@patch 1.00
*/
function MeleeInitVictoryDefeat takes nothing returns nothing
    local trigger    trig
    local integer    index
    local player     indexPlayer

    // Create a timer window for the "finish soon" timeout period, it has no timer
    // because it is driven by real time (outside of the game state to avoid desyncs)
    set bj_finishSoonTimerDialog = CreateTimerDialog(null)

    // Set a trigger to fire when we receive a "finish soon" game event
    set trig = CreateTrigger()
    call TriggerRegisterGameEvent(trig, EVENT_GAME_TOURNAMENT_FINISH_SOON)
    call TriggerAddAction(trig, function MeleeTriggerTournamentFinishSoon)

    // Set a trigger to fire when we receive a "finish now" game event
    set trig = CreateTrigger()
    call TriggerRegisterGameEvent(trig, EVENT_GAME_TOURNAMENT_FINISH_NOW)
    call TriggerAddAction(trig, function MeleeTriggerTournamentFinishNow)

    // Set up each player's mortality code.
    set index = 0
    loop
        set indexPlayer = Player(index)

        // Make sure this player slot is playing.
        if (GetPlayerSlotState(indexPlayer) == PLAYER_SLOT_STATE_PLAYING) then
            set bj_meleeDefeated[index] = false
            set bj_meleeVictoried[index] = false

            // Create a timer and timer window in case the player is crippled.
            set bj_playerIsCrippled[index] = false
            set bj_playerIsExposed[index] = false
            set bj_crippledTimer[index] = CreateTimer()
            set bj_crippledTimerWindows[index] = CreateTimerDialog(bj_crippledTimer[index])
            call TimerDialogSetTitle(bj_crippledTimerWindows[index], MeleeGetCrippledTimerMessage(indexPlayer))

            // Set a trigger to fire whenever a building is cancelled for this player.
            set trig = CreateTrigger()
            call TriggerRegisterPlayerUnitEvent(trig, indexPlayer, EVENT_PLAYER_UNIT_CONSTRUCT_CANCEL, null)
            call TriggerAddAction(trig, function MeleeTriggerActionConstructCancel)

            // Set a trigger to fire whenever a unit dies for this player.
            set trig = CreateTrigger()
            call TriggerRegisterPlayerUnitEvent(trig, indexPlayer, EVENT_PLAYER_UNIT_DEATH, null)
            call TriggerAddAction(trig, function MeleeTriggerActionUnitDeath)

            // Set a trigger to fire whenever a unit begins construction for this player
            set trig = CreateTrigger()
            call TriggerRegisterPlayerUnitEvent(trig, indexPlayer, EVENT_PLAYER_UNIT_CONSTRUCT_START, null)
            call TriggerAddAction(trig, function MeleeTriggerActionUnitConstructionStart)

            // Set a trigger to fire whenever this player defeats-out
            set trig = CreateTrigger()
            call TriggerRegisterPlayerEvent(trig, indexPlayer, EVENT_PLAYER_DEFEAT)
            call TriggerAddAction(trig, function MeleeTriggerActionPlayerDefeated)

            // Set a trigger to fire whenever this player leaves
            set trig = CreateTrigger()
            call TriggerRegisterPlayerEvent(trig, indexPlayer, EVENT_PLAYER_LEAVE)
            call TriggerAddAction(trig, function MeleeTriggerActionPlayerLeft)

            // Set a trigger to fire whenever this player changes his/her alliances.
            set trig = CreateTrigger()
            call TriggerRegisterPlayerAllianceChange(trig, indexPlayer, ALLIANCE_PASSIVE)
            call TriggerRegisterPlayerStateEvent(trig, indexPlayer, PLAYER_STATE_ALLIED_VICTORY, EQUAL, 1)
            call TriggerAddAction(trig, function MeleeTriggerActionAllianceChange)
        else
            set bj_meleeDefeated[index] = true
            set bj_meleeVictoried[index] = false

            // Handle leave events for observers
            if (IsPlayerObserver(indexPlayer)) then
                // Set a trigger to fire whenever this player leaves
                set trig = CreateTrigger()
                call TriggerRegisterPlayerEvent(trig, indexPlayer, EVENT_PLAYER_LEAVE)
                call TriggerAddAction(trig, function MeleeTriggerActionPlayerLeft)
            endif
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop

    // Test for victory / defeat at startup, in case the user has already won / lost.
    // Allow for a short time to pass first, so that the map can finish loading.
    call TimerStart(CreateTimer(), 2.0, false, function MeleeTriggerActionAllianceChange)
endfunction



//***************************************************************************
//*
//*  Player Slot Availability
//*
//***************************************************************************

//===========================================================================

/**
This function will pre-initialize `bj_slotControlUsed` and `bj_slotControl` arrays, representing player slots, with default values of `false` and `MAP_CONTROL_USER`.

@note It is safe to call this function repeatedly, it will only ever run the
initialization once.

@patch 1.00
*/
function CheckInitPlayerSlotAvailability takes nothing returns nothing
    local integer index

    if (not bj_slotControlReady) then
        set index = 0
        loop
            set bj_slotControlUsed[index] = false
            set bj_slotControl[index] = MAP_CONTROL_USER
            set index = index + 1
            exitwhen index == bj_MAX_PLAYERS
        endloop
        set bj_slotControlReady = true
    endif
endfunction

//===========================================================================

/**
This function is called in the scope of `config` to save map's slots settings in
two arrays:

- `bj_slotControlUsed` (index playerId, zero-based): `true` if player is defined in config (defaults to `false`).
- `bj_slotControl` (index playerId, zero-based): value is of type `mapcontrol` for
user/computer-controlled player (defaults to `MAP_CONTROL_USER`).

@param whichPlayer Initialize target player

@param control Player's slot type

@patch 1.00
*/
function SetPlayerSlotAvailable takes player whichPlayer, mapcontrol control returns nothing
    local integer playerIndex = GetPlayerId(whichPlayer)

    call CheckInitPlayerSlotAvailability()
    set bj_slotControlUsed[playerIndex] = true
    set bj_slotControl[playerIndex] = control
endfunction



//***************************************************************************
//*
//*  Generic Template Player-slot Initialization
//*
//***************************************************************************

//===========================================================================

/**
Assign all players to a team (force) in a "pseudo-random" fashion.

@note Players aren't assigned to teams randomly, but in a round-robin way.
This means if `teamCount == 2` with 4 players:

- player 0 -> team 0
- player 1 -> team 1
- player 2 -> team 0
- player 3 -> team 1
- etc.

@note This function is called in the scope of `config` by `InitGenericPlayerSlots` etc. to set up the map based on chosen *default game type*.

@patch 1.00
*/
function TeamInitPlayerSlots takes integer teamCount returns nothing
    local integer index
    local player  indexPlayer
    local integer team

    call SetTeams(teamCount)

    call CheckInitPlayerSlotAvailability()
    set index = 0
    set team = 0
    loop
        if (bj_slotControlUsed[index]) then
            set indexPlayer = Player(index)
            call SetPlayerTeam( indexPlayer, team )
            set team = team + 1
            if (team >= teamCount) then
                set team = 0
            endif
        endif

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================

/**
This function is called in the scope of `config` by `InitGenericPlayerSlots` to set up the map based on chosen game type `GAME_TYPE_MELEE`.

@patch 1.00
*/
function MeleeInitPlayerSlots takes nothing returns nothing
    call TeamInitPlayerSlots(bj_MAX_PLAYERS)
endfunction

//===========================================================================

/**
This function is called in the scope of `config` by `InitGenericPlayerSlots` to set up the map based on chosen game type `GAME_TYPE_FFA`.

@patch 1.00
*/
function FFAInitPlayerSlots takes nothing returns nothing
    call TeamInitPlayerSlots(bj_MAX_PLAYERS)
endfunction

//===========================================================================

/**
This function is called in the scope of `config` by `InitGenericPlayerSlots` to set up the map based on chosen game type `GAME_TYPE_ONE_ON_ONE`.

@patch 1.00
*/
function OneOnOneInitPlayerSlots takes nothing returns nothing
    // Limit the game to 2 players.
    call SetTeams(2)
    call SetPlayers(2)
    call TeamInitPlayerSlots(2)
endfunction

//===========================================================================

/**
This function is called in the scope of `config` to set up the map based on
game type. Specifically, this assigns players into teams, unless the
game type is `GAME_TYPE_USE_MAP_SETTINGS` (then map-specific code does this)
or unknown (nothing is done in that case).

@bug Leaks handle `gType`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function InitGenericPlayerSlots takes nothing returns nothing
    local gametype gType = GetGameTypeSelected()

    if (gType == GAME_TYPE_MELEE) then
        call MeleeInitPlayerSlots()
    elseif (gType == GAME_TYPE_FFA) then
        call FFAInitPlayerSlots()
    elseif (gType == GAME_TYPE_USE_MAP_SETTINGS) then
        // Do nothing; the map-specific script handles this.
    elseif (gType == GAME_TYPE_ONE_ON_ONE) then
        call OneOnOneInitPlayerSlots()
    elseif (gType == GAME_TYPE_TWO_TEAM_PLAY) then
        call TeamInitPlayerSlots(2)
    elseif (gType == GAME_TYPE_THREE_TEAM_PLAY) then
        call TeamInitPlayerSlots(3)
    elseif (gType == GAME_TYPE_FOUR_TEAM_PLAY) then
        call TeamInitPlayerSlots(4)
    else
        // Unrecognized Game Type
    endif
endfunction



//***************************************************************************
//*
//*  Blizzard.j Initialization
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function SetDNCSoundsDawn takes nothing returns nothing
    if bj_useDawnDuskSounds then
        call StartSound(bj_dawnSound)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDNCSoundsDusk takes nothing returns nothing
    if bj_useDawnDuskSounds then
        call StartSound(bj_duskSound)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDNCSoundsDay takes nothing returns nothing
    local real ToD = GetTimeOfDay()

    if (ToD >= bj_TOD_DAWN and ToD < bj_TOD_DUSK) and not bj_dncIsDaytime then
        set bj_dncIsDaytime = true

        // change ambient sounds
        call StopSound(bj_nightAmbientSound, false, true)
        call StartSound(bj_dayAmbientSound)
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function SetDNCSoundsNight takes nothing returns nothing
    local real ToD = GetTimeOfDay()

    if (ToD < bj_TOD_DAWN or ToD >= bj_TOD_DUSK) and bj_dncIsDaytime then
        set bj_dncIsDaytime = false

        // change ambient sounds
        call StopSound(bj_dayAmbientSound, false, true)
        call StartSound(bj_nightAmbientSound)
    endif
endfunction

//===========================================================================

/**
Initializes day-night cycle sounds, global "bj_" variables and triggers.

@patch 1.00
*/
function InitDNCSounds takes nothing returns nothing
    // Create sounds to be played at dawn and dusk.
    set bj_dawnSound = CreateSoundFromLabel("RoosterSound", false, false, false, 10000, 10000)
    set bj_duskSound = CreateSoundFromLabel("WolfSound", false, false, false, 10000, 10000)

    // Set up triggers to respond to dawn and dusk.
    set bj_dncSoundsDawn = CreateTrigger()
    call TriggerRegisterGameStateEvent(bj_dncSoundsDawn, GAME_STATE_TIME_OF_DAY, EQUAL, bj_TOD_DAWN)
    call TriggerAddAction(bj_dncSoundsDawn, function SetDNCSoundsDawn)

    set bj_dncSoundsDusk = CreateTrigger()
    call TriggerRegisterGameStateEvent(bj_dncSoundsDusk, GAME_STATE_TIME_OF_DAY, EQUAL, bj_TOD_DUSK)
    call TriggerAddAction(bj_dncSoundsDusk, function SetDNCSoundsDusk)

    // Set up triggers to respond to changes from day to night or vice-versa.
    set bj_dncSoundsDay = CreateTrigger()
    call TriggerRegisterGameStateEvent(bj_dncSoundsDay,   GAME_STATE_TIME_OF_DAY, GREATER_THAN_OR_EQUAL, bj_TOD_DAWN)
    call TriggerRegisterGameStateEvent(bj_dncSoundsDay,   GAME_STATE_TIME_OF_DAY, LESS_THAN,             bj_TOD_DUSK)
    call TriggerAddAction(bj_dncSoundsDay, function SetDNCSoundsDay)

    set bj_dncSoundsNight = CreateTrigger()
    call TriggerRegisterGameStateEvent(bj_dncSoundsNight, GAME_STATE_TIME_OF_DAY, LESS_THAN,             bj_TOD_DAWN)
    call TriggerRegisterGameStateEvent(bj_dncSoundsNight, GAME_STATE_TIME_OF_DAY, GREATER_THAN_OR_EQUAL, bj_TOD_DUSK)
    call TriggerAddAction(bj_dncSoundsNight, function SetDNCSoundsNight)
endfunction

//===========================================================================

/**
@bug Leaks handle `v`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function InitBlizzardGlobals takes nothing returns nothing
    local integer index
    local integer userControlledPlayers
    local version v

    // Init filter function vars
    set filterIssueHauntOrderAtLocBJ = Filter(function IssueHauntOrderAtLocBJFilter)
    set filterEnumDestructablesInCircleBJ = Filter(function EnumDestructablesInCircleBJFilter)
    set filterGetUnitsInRectOfPlayer = Filter(function GetUnitsInRectOfPlayerFilter)
    set filterGetUnitsOfTypeIdAll = Filter(function GetUnitsOfTypeIdAllFilter)
    set filterGetUnitsOfPlayerAndTypeId = Filter(function GetUnitsOfPlayerAndTypeIdFilter)
    set filterMeleeTrainedUnitIsHeroBJ = Filter(function MeleeTrainedUnitIsHeroBJFilter)
    set filterLivingPlayerUnitsOfTypeId = Filter(function LivingPlayerUnitsOfTypeIdFilter)

    // Init force presets
    set index = 0
    loop
        exitwhen index == bj_MAX_PLAYER_SLOTS
        set bj_FORCE_PLAYER[index] = CreateForce()
        call ForceAddPlayer(bj_FORCE_PLAYER[index], Player(index))
        set index = index + 1
    endloop

    set bj_FORCE_ALL_PLAYERS = CreateForce()
    call ForceEnumPlayers(bj_FORCE_ALL_PLAYERS, null)

    // Init Cinematic Mode history
    set bj_cineModePriorSpeed = GetGameSpeed()
    set bj_cineModePriorFogSetting = IsFogEnabled()
    set bj_cineModePriorMaskSetting = IsFogMaskEnabled()

    // Init Trigger Queue
    set index = 0
    loop
        exitwhen index >= bj_MAX_QUEUED_TRIGGERS
        set bj_queuedExecTriggers[index] = null
        set bj_queuedExecUseConds[index] = false
        set index = index + 1
    endloop

    // Init singleplayer check
    set bj_isSinglePlayer = false
    set userControlledPlayers = 0
    set index = 0
    loop
        exitwhen index >= bj_MAX_PLAYERS
        if (GetPlayerController(Player(index)) == MAP_CONTROL_USER and GetPlayerSlotState(Player(index)) == PLAYER_SLOT_STATE_PLAYING) then
            set userControlledPlayers = userControlledPlayers + 1
        endif
        set index = index + 1
    endloop
    set bj_isSinglePlayer = (userControlledPlayers == 1)

    // Init sounds
    //set bj_pingMinimapSound = CreateSoundFromLabel("AutoCastButtonClick", false, false, false, 10000, 10000)
    set bj_rescueSound = CreateSoundFromLabel("Rescue", false, false, false, 10000, 10000)
    set bj_questDiscoveredSound = CreateSoundFromLabel("QuestNew", false, false, false, 10000, 10000)
    set bj_questUpdatedSound = CreateSoundFromLabel("QuestUpdate", false, false, false, 10000, 10000)
    set bj_questCompletedSound = CreateSoundFromLabel("QuestCompleted", false, false, false, 10000, 10000)
    set bj_questFailedSound = CreateSoundFromLabel("QuestFailed", false, false, false, 10000, 10000)
    set bj_questHintSound = CreateSoundFromLabel("Hint", false, false, false, 10000, 10000)
    set bj_questSecretSound = CreateSoundFromLabel("SecretFound", false, false, false, 10000, 10000)
    set bj_questItemAcquiredSound = CreateSoundFromLabel("ItemReward", false, false, false, 10000, 10000)
    set bj_questWarningSound = CreateSoundFromLabel("Warning", false, false, false, 10000, 10000)
    set bj_victoryDialogSound = CreateSoundFromLabel("QuestCompleted", false, false, false, 10000, 10000)
    set bj_defeatDialogSound = CreateSoundFromLabel("QuestFailed", false, false, false, 10000, 10000)

    // Init corpse creation triggers.
    call DelayedSuspendDecayCreate()

    // Init version-specific data
    set v = VersionGet()
    if (v == VERSION_REIGN_OF_CHAOS) then
        set bj_MELEE_MAX_TWINKED_HEROES = bj_MELEE_MAX_TWINKED_HEROES_V0
    else
        set bj_MELEE_MAX_TWINKED_HEROES = bj_MELEE_MAX_TWINKED_HEROES_V1
    endif
endfunction

//===========================================================================

/**
@patch 1.00
*/
function InitQueuedTriggers takes nothing returns nothing
    set bj_queuedExecTimeout = CreateTrigger()
    call TriggerRegisterTimerExpireEvent(bj_queuedExecTimeout, bj_queuedExecTimeoutTimer)
    call TriggerAddAction(bj_queuedExecTimeout, function QueuedTriggerDoneBJ)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function InitMapRects takes nothing returns nothing
    set bj_mapInitialPlayableArea = Rect(GetCameraBoundMinX()-GetCameraMargin(CAMERA_MARGIN_LEFT), GetCameraBoundMinY()-GetCameraMargin(CAMERA_MARGIN_BOTTOM), GetCameraBoundMaxX()+GetCameraMargin(CAMERA_MARGIN_RIGHT), GetCameraBoundMaxY()+GetCameraMargin(CAMERA_MARGIN_TOP))
    set bj_mapInitialCameraBounds = GetCurrentCameraBoundsMapRectBJ()
endfunction

//===========================================================================

/**
@patch 1.00
*/
function InitSummonableCaps takes nothing returns nothing
    local integer index

    set index = 0
    loop
        // upgraded units
        // Note: Only do this if the corresponding upgrade is not yet researched
        // Barrage - Siege Engines
        if (not GetPlayerTechResearched(Player(index), 'Rhrt', true)) then
            call SetPlayerTechMaxAllowed(Player(index), 'hrtt', 0)
        endif

        // Berserker Upgrade - Troll Berserkers
        if (not GetPlayerTechResearched(Player(index), 'Robk', true)) then
            call SetPlayerTechMaxAllowed(Player(index), 'otbk', 0)
        endif

        // max skeletons per player
        call SetPlayerTechMaxAllowed(Player(index), 'uske', bj_MAX_SKELETONS)

        set index = index + 1
        exitwhen index == bj_MAX_PLAYERS
    endloop
endfunction

//===========================================================================
// Update the per-class stock limits.
//

/**
@bug Leaks handle `iType`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function UpdateStockAvailability takes item whichItem returns nothing
    local itemtype iType  = GetItemType(whichItem)
    local integer  iLevel = GetItemLevel(whichItem)

    // Update allowed type/level combinations.
    if (iType == ITEM_TYPE_PERMANENT) then
        set bj_stockAllowedPermanent[iLevel] = true
    elseif (iType == ITEM_TYPE_CHARGED) then
        set bj_stockAllowedCharged[iLevel] = true
    elseif (iType == ITEM_TYPE_ARTIFACT) then
        set bj_stockAllowedArtifact[iLevel] = true
    else
        // Not interested in this item type - ignore the item.
    endif
endfunction

//===========================================================================
// Find a sellable item of the given type and level, and then add it.
//

/**
@patch 1.07
*/
function UpdateEachStockBuildingEnum takes nothing returns nothing
    local integer iteration = 0
    local integer pickedItemId

    loop
        set pickedItemId = ChooseRandomItemEx(bj_stockPickedItemType, bj_stockPickedItemLevel)
        exitwhen IsItemIdSellable(pickedItemId)

        // If we get hung up on an entire class/level combo of unsellable
        // items, or a very unlucky series of random numbers, give up.
        set iteration = iteration + 1
        if (iteration > bj_STOCK_MAX_ITERATIONS) then
            return
        endif
    endloop
    call AddItemToStock(GetEnumUnit(), pickedItemId, 1, 1)
endfunction

//===========================================================================

/**
@bug Leaks handle `g`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function UpdateEachStockBuilding takes itemtype iType, integer iLevel returns nothing
    local group g

    set bj_stockPickedItemType = iType
    set bj_stockPickedItemLevel = iLevel

    set g = CreateGroup()
    call GroupEnumUnitsOfType(g, "marketplace", null)
    call ForGroup(g, function UpdateEachStockBuildingEnum)
    call DestroyGroup(g)
endfunction

//===========================================================================
// Update stock inventory.
//

/**
@bug Leaks handle `pickedItemType`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.07
*/
function PerformStockUpdates takes nothing returns nothing
    local integer  pickedItemId
    local itemtype pickedItemType
    local integer  pickedItemLevel = 0
    local integer  allowedCombinations = 0
    local integer  iLevel

    // Give each type/level combination a chance of being picked.
    set iLevel = 1
    loop
        if (bj_stockAllowedPermanent[iLevel]) then
            set allowedCombinations = allowedCombinations + 1
            if (GetRandomInt(1, allowedCombinations) == 1) then
                set pickedItemType = ITEM_TYPE_PERMANENT
                set pickedItemLevel = iLevel
            endif
        endif
        if (bj_stockAllowedCharged[iLevel]) then
            set allowedCombinations = allowedCombinations + 1
            if (GetRandomInt(1, allowedCombinations) == 1) then
                set pickedItemType = ITEM_TYPE_CHARGED
                set pickedItemLevel = iLevel
            endif
        endif
        if (bj_stockAllowedArtifact[iLevel]) then
            set allowedCombinations = allowedCombinations + 1
            if (GetRandomInt(1, allowedCombinations) == 1) then
                set pickedItemType = ITEM_TYPE_ARTIFACT
                set pickedItemLevel = iLevel
            endif
        endif

        set iLevel = iLevel + 1
        exitwhen iLevel > bj_MAX_ITEM_LEVEL
    endloop

    // Make sure we found a valid item type to add.
    if (allowedCombinations == 0) then
        return
    endif

    call UpdateEachStockBuilding(pickedItemType, pickedItemLevel)
endfunction

//===========================================================================
// Perform the first update, and then arrange future updates.
//

/**
@patch 1.07
*/
function StartStockUpdates takes nothing returns nothing
    call PerformStockUpdates()
    call TimerStart(bj_stockUpdateTimer, bj_STOCK_RESTOCK_INTERVAL, true, function PerformStockUpdates)
endfunction

//===========================================================================

/**
@patch 1.07
*/
function RemovePurchasedItem takes nothing returns nothing
    call RemoveItemFromStock(GetSellingUnit(), GetItemTypeId(GetSoldItem()))
endfunction

//===========================================================================

/**
@patch 1.07
*/
function InitNeutralBuildings takes nothing returns nothing
    local integer iLevel

    // Chart of allowed stock items.
    set iLevel = 0
    loop
        set bj_stockAllowedPermanent[iLevel] = false
        set bj_stockAllowedCharged[iLevel] = false
        set bj_stockAllowedArtifact[iLevel] = false
        set iLevel = iLevel + 1
        exitwhen iLevel > bj_MAX_ITEM_LEVEL
    endloop

    // Limit stock inventory slots.
    call SetAllItemTypeSlots(bj_MAX_STOCK_ITEM_SLOTS)
    call SetAllUnitTypeSlots(bj_MAX_STOCK_UNIT_SLOTS)

    // Arrange the first update.
    set bj_stockUpdateTimer = CreateTimer()
    call TimerStart(bj_stockUpdateTimer, bj_STOCK_RESTOCK_INITIAL_DELAY, false, function StartStockUpdates)

    // Set up a trigger to fire whenever an item is sold.
    set bj_stockItemPurchased = CreateTrigger()
    call TriggerRegisterPlayerUnitEvent(bj_stockItemPurchased, Player(PLAYER_NEUTRAL_PASSIVE), EVENT_PLAYER_UNIT_SELL_ITEM, null)
    call TriggerAddAction(bj_stockItemPurchased, function RemovePurchasedItem)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function MarkGameStarted takes nothing returns nothing
    set bj_gameStarted = true
    call DestroyTimer(bj_gameStartedTimer)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function DetectGameStarted takes nothing returns nothing
    set bj_gameStartedTimer = CreateTimer()
    call TimerStart(bj_gameStartedTimer, bj_GAME_STARTED_THRESHOLD, false, function MarkGameStarted)
endfunction

//===========================================================================

/**
@patch 1.00
*/
function InitBlizzard takes nothing returns nothing
    // Set up the Neutral Victim player slot, to torture the abandoned units
    // of defeated players.  Since some triggers expect this player slot to
    // exist, this is performed for all maps.
    call ConfigureNeutralVictim()

    call InitBlizzardGlobals()
    call InitQueuedTriggers()
    call InitRescuableBehaviorBJ()
    call InitDNCSounds()
    call InitMapRects()
    call InitSummonableCaps()
    call InitNeutralBuildings()
    call DetectGameStarted()
endfunction



//***************************************************************************
//*
//*  Random distribution
//*
//*  Used to select a random object from a given distribution of chances
//*
//*  - RandomDistReset clears the distribution list
//*
//*  - RandomDistAddItem adds a new object to the distribution list
//*    with a given identifier and an integer chance to be chosen
//*
//*  - RandomDistChoose will use the current distribution list to choose
//*    one of the objects randomly based on the chance distribution
//*  
//*  Note that the chances are effectively normalized by their sum,
//*  so only the relative values of each chance are important
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.00
*/
function RandomDistReset takes nothing returns nothing
    set bj_randDistCount = 0
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RandomDistAddItem takes integer inID, integer inChance returns nothing
    set bj_randDistID[bj_randDistCount] = inID
    set bj_randDistChance[bj_randDistCount] = inChance
    set bj_randDistCount = bj_randDistCount + 1
endfunction

//===========================================================================

/**
@patch 1.00
*/
function RandomDistChoose takes nothing returns integer
    local integer sum = 0
    local integer chance = 0
    local integer index
    local integer foundID = -1
    local boolean done

    // No items?
    if (bj_randDistCount == 0) then
        return -1
    endif

    // Find sum of all chances
    set index = 0
    loop
        set sum = sum + bj_randDistChance[index]

        set index = index + 1
        exitwhen index == bj_randDistCount
    endloop

    // Choose random number within the total range
    set chance = GetRandomInt(1, sum)

    // Find ID which corresponds to this chance
    set index = 0
    set sum = 0
    set done = false
    loop
        set sum = sum + bj_randDistChance[index]

        if (chance <= sum) then
            set foundID = bj_randDistID[index]
            set done = true
        endif

        set index = index + 1
        if (index == bj_randDistCount) then
            set done = true
        endif

        exitwhen done == true
    endloop

    return foundID
endfunction



//***************************************************************************
//*
//*  Drop item
//*
//*  Makes the given unit drop the given item
//*
//*  Note: This could potentially cause problems if the unit is standing
//*        right on the edge of an unpathable area and happens to drop the
//*        item into the unpathable area where nobody can get it...
//*
//***************************************************************************


/**
@bug Leaks handle `droppedItem`: In Jass you must set local variables that hold agents (or any child type) to `null` at the end of functions to avoid reference counter leaks.

@patch 1.00
*/
function UnitDropItem takes unit inUnit, integer inItemID returns item
    local real x
    local real y
    local real radius = 32
    local real unitX
    local real unitY
    local item droppedItem

    if (inItemID == -1) then
        return null
    endif

    set unitX = GetUnitX(inUnit)
    set unitY = GetUnitY(inUnit)

    set x = GetRandomReal(unitX - radius, unitX + radius)
    set y = GetRandomReal(unitY - radius, unitY + radius)

    set droppedItem = CreateItem(inItemID, x, y)

    call SetItemDropID(droppedItem, GetUnitTypeId(inUnit))
    call UpdateStockAvailability(droppedItem)

    return droppedItem
endfunction

//===========================================================================

/**
@patch 1.07
*/
function WidgetDropItem takes widget inWidget, integer inItemID returns item
    local real x
    local real y
    local real radius = 32
    local real widgetX
    local real widgetY

    if (inItemID == -1) then
        return null
    endif

    set widgetX = GetWidgetX(inWidget)
    set widgetY = GetWidgetY(inWidget)

    set x = GetRandomReal(widgetX - radius, widgetX + radius)
    set y = GetRandomReal(widgetY - radius, widgetY + radius)

    return CreateItem(inItemID, x, y)
endfunction


//***************************************************************************
//*
//*  Instanced Object Operation Functions
//*
//*  Get/Set specific fields for single unit/item/ability instance
//*
//***************************************************************************

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzIsLastInstanceObjectFunctionSuccessful takes nothing returns boolean
    return bj_lastInstObjFuncSuccessful
endfunction

// Ability
//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityBooleanFieldBJ takes ability whichAbility, abilitybooleanfield whichField, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityBooleanField(whichAbility, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityIntegerFieldBJ takes ability whichAbility, abilityintegerfield whichField, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityIntegerField(whichAbility, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityRealFieldBJ takes ability whichAbility, abilityrealfield whichField, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityRealField(whichAbility, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityStringFieldBJ takes ability whichAbility, abilitystringfield whichField, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityStringField(whichAbility, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityBooleanLevelFieldBJ takes ability whichAbility, abilitybooleanlevelfield whichField, integer level, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityBooleanLevelField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityIntegerLevelFieldBJ takes ability whichAbility, abilityintegerlevelfield whichField, integer level, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityIntegerLevelField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityRealLevelFieldBJ takes ability whichAbility, abilityreallevelfield whichField, integer level, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityRealLevelField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityStringLevelFieldBJ takes ability whichAbility, abilitystringlevelfield whichField, integer level, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityStringLevelField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityBooleanLevelArrayFieldBJ takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, integer index, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityBooleanLevelArrayField(whichAbility, whichField, level, index, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityIntegerLevelArrayFieldBJ takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer index, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityIntegerLevelArrayField(whichAbility, whichField, level, index, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityRealLevelArrayFieldBJ takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, integer index, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityRealLevelArrayField(whichAbility, whichField, level, index, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetAbilityStringLevelArrayFieldBJ takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, integer index, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetAbilityStringLevelArrayField(whichAbility, whichField, level, index, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzAddAbilityBooleanLevelArrayFieldBJ takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzAddAbilityBooleanLevelArrayField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzAddAbilityIntegerLevelArrayFieldBJ takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzAddAbilityIntegerLevelArrayField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzAddAbilityRealLevelArrayFieldBJ takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzAddAbilityRealLevelArrayField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzAddAbilityStringLevelArrayFieldBJ takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzAddAbilityStringLevelArrayField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzRemoveAbilityBooleanLevelArrayFieldBJ takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzRemoveAbilityBooleanLevelArrayField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzRemoveAbilityIntegerLevelArrayFieldBJ takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzRemoveAbilityIntegerLevelArrayField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzRemoveAbilityRealLevelArrayFieldBJ takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzRemoveAbilityRealLevelArrayField(whichAbility, whichField, level, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzRemoveAbilityStringLevelArrayFieldBJ takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzRemoveAbilityStringLevelArrayField(whichAbility, whichField, level, value)
endfunction

// Item 
//=============================================================

/**
@patch 1.31.0.11889
*/
function BlzItemAddAbilityBJ takes item whichItem, integer abilCode returns nothing
    set bj_lastInstObjFuncSuccessful = BlzItemAddAbility(whichItem, abilCode)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzItemRemoveAbilityBJ takes item whichItem, integer abilCode returns nothing
    set bj_lastInstObjFuncSuccessful = BlzItemRemoveAbility(whichItem, abilCode)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetItemBooleanFieldBJ takes item whichItem, itembooleanfield whichField, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetItemBooleanField(whichItem, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetItemIntegerFieldBJ takes item whichItem, itemintegerfield whichField, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetItemIntegerField(whichItem, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetItemRealFieldBJ takes item whichItem, itemrealfield whichField, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetItemRealField(whichItem, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetItemStringFieldBJ takes item whichItem, itemstringfield whichField, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetItemStringField(whichItem, whichField, value)
endfunction


// Unit 
//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitBooleanFieldBJ takes unit whichUnit, unitbooleanfield whichField, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitBooleanField(whichUnit, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitIntegerFieldBJ takes unit whichUnit, unitintegerfield whichField, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitIntegerField(whichUnit, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitRealFieldBJ takes unit whichUnit, unitrealfield whichField, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitRealField(whichUnit, whichField, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitStringFieldBJ takes unit whichUnit, unitstringfield whichField, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitStringField(whichUnit, whichField, value)
endfunction

// Unit Weapon
//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitWeaponBooleanFieldBJ takes unit whichUnit, unitweaponbooleanfield whichField, integer index, boolean value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitWeaponBooleanField(whichUnit, whichField, index, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitWeaponIntegerFieldBJ takes unit whichUnit, unitweaponintegerfield whichField, integer index, integer value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitWeaponIntegerField(whichUnit, whichField, index, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitWeaponRealFieldBJ takes unit whichUnit, unitweaponrealfield whichField, integer index, real value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitWeaponRealField(whichUnit, whichField, index, value)
endfunction

//===========================================================================

/**
@patch 1.31.0.11889
*/
function BlzSetUnitWeaponStringFieldBJ takes unit whichUnit, unitweaponstringfield whichField, integer index, string value returns nothing
    set bj_lastInstObjFuncSuccessful = BlzSetUnitWeaponStringField(whichUnit, whichField, index, value)
endfunction
