// Force API

native CreateForce              takes nothing returns force

native DestroyForce             takes force whichForce returns nothing

native ForceAddPlayer           takes force whichForce, player whichPlayer returns nothing

native ForceRemovePlayer        takes force whichForce, player whichPlayer returns nothing

native ForceClear               takes force whichForce returns nothing

native ForceEnumPlayers         takes force whichForce, boolexpr filter returns nothing

native ForceEnumPlayersCounted  takes force whichForce, boolexpr filter, integer countLimit returns nothing

native ForceEnumAllies          takes force whichForce, player whichPlayer, boolexpr filter returns nothing

native ForceEnumEnemies         takes force whichForce, player whichPlayer, boolexpr filter returns nothing

native ForForce                 takes force whichForce, code callback returns nothing