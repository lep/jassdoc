// Creates a new or reads in an existing game cache file stored

// in the current campaign profile dir

//

native  ReloadGameCachesFromDisk takes nothing returns boolean


/**
@note You cannot create more than 255 gamecaches.
In multiplayer the existing game caches are not considered, so you can get a
full 255 new game caches.

In singleplayer, when you call `InitGameCache`, it looks in the Campaigns.w3v
file if a `gamecache` with that name already exists, if yes, it will create a
`gamecache` handle (you can get multiple handles for the same game cache, and
that will only count once to the 255 limit in the current game), if no and it
does not exist yet in the current game either, it will take a new slot among
the 255.
*/
native  InitGameCache    takes string campaignFile returns gamecache

native  SaveGameCache    takes gamecache whichCache returns boolean



native  StoreInteger					takes gamecache cache, string missionKey, string key, integer value returns nothing

native  StoreReal						takes gamecache cache, string missionKey, string key, real value returns nothing

native  StoreBoolean					takes gamecache cache, string missionKey, string key, boolean value returns nothing

/**
Stores a description of a unit in a game cache that can be retrieved with `RestoreUnit`.

The saved attributes of the unit are (non-exhaustive): unitId, experience, hero level, unused skill points, hero proper name (index),
strength/agility/intelligence, attack speed/move speed increments from agility, life, mana and attack damage increments
(can be adjusted individually using tome abilities), sight range (day) (can be adjusted with UNIT_RF_SIGHT_RADIUS), armor increment

Descriptions of the items in the unit's inventory will also be saved (non-exhaustive): itemId, charges, flags: drop upon death, perishable,
invulnerable, pawnable, used on acquire (powerup), droppable, actively used

Descriptions of the unit's hero abilities will also be saved: abilityId, currentLevel

See also https://github.com/WaterKnight/Warcraft3-Formats-KaitaiStruct/blob/main/w3-w3v.ksy

@bug When a unit obtains armor from a research and is then stored in a game cache, restoring it will retain the armor increment without the research, so if
the research is done again, the unit will benefit doubly.

@bug If a hero unit was stored under some key pair and later the key pair is overwritten with a non-hero unit, the previous hero attributes will not
be overwritten, i.e., they will remain and be merged with the non-hero attributes. This can be observed in the persisted .w3v file. Ingame, it
would not make a difference because the restored non-hero unit normally would not use the hero attributes.
*/
native  StoreUnit						takes gamecache cache, string missionKey, string key, unit whichUnit returns boolean

native  StoreString						takes gamecache cache, string missionKey, string key, string value returns boolean



/**
Synchronizes the value stored in the `gamecache` under the mission key and key.
Calling this function sends a sync packet from each player in the calling
context (citation needed), that is everybody sends a packet to everybody.
The game then picks the first packet arrived (at the host). Often (but not
always) that is the packet coming from the game host.

More interesting perhaps is the use to synchronize local data (like a player's
camera position) to all other players. To do this only store and sync the value
in a local context:

```
if GetLocalPlayer() == p then
    call StoreInteger(my_cache, "missionkey", "key", my_value)
    call SyncStoredInteger(my_cache, "missionkey", "key")
endif
```

Now this will synchronize the local value `my_value` to each player but we don't
know when each player has actually received it. You can use `TriggerSyncReady`
to wait for each sync action, but it's not recommended as it is very slow and
can hang for minutes (cf. [sync doc](https://www.hiveworkshop.com/pastebin/1ce4fe042832e6bd7d06697a43055373.5801))
Instead it is recommended to use a rapid timer to check if the key is present
in the gamecache. Note that this is still a local operation as different players
can receive the sync at different times. If a player has received all the data
you synchronize the fact that that player has got all the data. This is
reasonably done via `SelectUnit` and
`TriggerRegisterPlayerUnitEvent(trig, p, EVENT_PLAYER_UNIT_SELECTED, null)`.
Now once the last player has sent their selection event you have synchronized
your data.

This is a very high-level overview and the process has many edges to look out
for, so it's probably a good idea to use an already made system like
[this one](https://www.hiveworkshop.com/threads/sync-game-cache.279148/).

@note You might rather use `BlzSendSyncData` if possible.

@note Calling multiple `SyncStoredX` in a row will keep their order in the
syncing process, i.e. first sync will be received first (FIFO).
*/
native SyncStoredInteger        takes gamecache cache, string missionKey, string key returns nothing

/**
Synchronizes the value stored in the `gamecache` under the mission key and key.
See `SyncStoredInteger` for a more in-depth explanation.
*/
native SyncStoredReal           takes gamecache cache, string missionKey, string key returns nothing

/**
Synchronizes the value stored in the `gamecache` under the mission key and key.
See `SyncStoredInteger` for a more in-depth explanation.
*/
native SyncStoredBoolean        takes gamecache cache, string missionKey, string key returns nothing

native SyncStoredUnit           takes gamecache cache, string missionKey, string key returns nothing

/**
@bug Does not seem to work.
*/
native SyncStoredString         takes gamecache cache, string missionKey, string key returns nothing



native  HaveStoredInteger					takes gamecache cache, string missionKey, string key returns boolean

native  HaveStoredReal						takes gamecache cache, string missionKey, string key returns boolean

native  HaveStoredBoolean					takes gamecache cache, string missionKey, string key returns boolean

native  HaveStoredUnit						takes gamecache cache, string missionKey, string key returns boolean

native  HaveStoredString					takes gamecache cache, string missionKey, string key returns boolean



native  FlushGameCache						takes gamecache cache returns nothing

native  FlushStoredMission					takes gamecache cache, string missionKey returns nothing

native  FlushStoredInteger					takes gamecache cache, string missionKey, string key returns nothing

native  FlushStoredReal						takes gamecache cache, string missionKey, string key returns nothing

native  FlushStoredBoolean					takes gamecache cache, string missionKey, string key returns nothing

native  FlushStoredUnit						takes gamecache cache, string missionKey, string key returns nothing

native  FlushStoredString					takes gamecache cache, string missionKey, string key returns nothing



/**
Returns `0` if the specified value's data is not found in the cache.
*/
native  GetStoredInteger				takes gamecache cache, string missionKey, string key returns integer

/**
Returns `0.0` if the specified value's data is not found in the cache.
*/
native  GetStoredReal					takes gamecache cache, string missionKey, string key returns real

/**
Returns `false` if the specified value's data is not found in the cache.
*/
native  GetStoredBoolean				takes gamecache cache, string missionKey, string key returns boolean

/**
Returns `""` if the specified value's data is not found in the cache.
*/
native  GetStoredString					takes gamecache cache, string missionKey, string key returns string

/**
Returns `null` if the specified value's data is not found in the cache.
*/
native  RestoreUnit						takes gamecache cache, string missionKey, string key, player forWhichPlayer, real x, real y, real facing returns unit
