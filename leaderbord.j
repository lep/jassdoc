// Leaderboard API



// Create a leaderboard object

native CreateLeaderboard                takes nothing returns leaderboard

native DestroyLeaderboard               takes leaderboard lb returns nothing



native LeaderboardDisplay               takes leaderboard lb, boolean show returns nothing

native IsLeaderboardDisplayed           takes leaderboard lb returns boolean



native LeaderboardGetItemCount          takes leaderboard lb returns integer



native LeaderboardSetSizeByItemCount    takes leaderboard lb, integer count returns nothing

native LeaderboardAddItem               takes leaderboard lb, string label, integer value, player p returns nothing

native LeaderboardRemoveItem            takes leaderboard lb, integer index returns nothing

native LeaderboardRemovePlayerItem      takes leaderboard lb, player p returns nothing

native LeaderboardClear                 takes leaderboard lb returns nothing



native LeaderboardSortItemsByValue      takes leaderboard lb, boolean ascending returns nothing

native LeaderboardSortItemsByPlayer     takes leaderboard lb, boolean ascending returns nothing

native LeaderboardSortItemsByLabel      takes leaderboard lb, boolean ascending returns nothing



native LeaderboardHasPlayerItem         takes leaderboard lb, player p returns boolean

native LeaderboardGetPlayerIndex        takes leaderboard lb, player p returns integer

native LeaderboardSetLabel              takes leaderboard lb, string label returns nothing

native LeaderboardGetLabelText          takes leaderboard lb returns string



native PlayerSetLeaderboard             takes player toPlayer, leaderboard lb returns nothing

native PlayerGetLeaderboard             takes player toPlayer returns leaderboard



native LeaderboardSetLabelColor         takes leaderboard lb, integer red, integer green, integer blue, integer alpha returns nothing

native LeaderboardSetValueColor         takes leaderboard lb, integer red, integer green, integer blue, integer alpha returns nothing

native LeaderboardSetStyle              takes leaderboard lb, boolean showLabel, boolean showNames, boolean showValues, boolean showIcons returns nothing



native LeaderboardSetItemValue          takes leaderboard lb, integer whichItem, integer val returns nothing

native LeaderboardSetItemLabel          takes leaderboard lb, integer whichItem, string val returns nothing

native LeaderboardSetItemStyle          takes leaderboard lb, integer whichItem, boolean showLabel, boolean showValue, boolean showIcon returns nothing

native LeaderboardSetItemLabelColor     takes leaderboard lb, integer whichItem, integer red, integer green, integer blue, integer alpha returns nothing

native LeaderboardSetItemValueColor     takes leaderboard lb, integer whichItem, integer red, integer green, integer blue, integer alpha returns nothing