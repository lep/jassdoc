// Item API

/**
Creates an item object at the specified coordinates ( x , y ).

@param itemid
The rawcode of the item.

@param x
The x-coordinate of the item

@param y
The y-coordinate of the item
*/
native CreateItem takes integer itemid, real x, real y returns item

native RemoveItem takes item whichItem returns nothing

native GetItemPlayer takes item whichItem returns player

native GetItemTypeId takes item i returns integer

native GetItemX takes item i returns real

native GetItemY takes item i returns real

native SetItemPosition takes item i, real x, real y returns nothing

native SetItemDropOnDeath takes item whichItem, boolean flag returns nothing

native SetItemDroppable takes item i, boolean flag returns nothing

native SetItemPawnable takes item i, boolean flag returns nothing

native SetItemPlayer takes item whichItem, player whichPlayer, boolean changeColor returns nothing

native SetItemInvulnerable takes item whichItem, boolean flag returns nothing

native IsItemInvulnerable takes item whichItem returns boolean

native SetItemVisible takes item whichItem, boolean show returns nothing

native IsItemVisible takes item whichItem returns boolean

native IsItemOwned takes item whichItem returns boolean

native IsItemPowerup takes item whichItem returns boolean

native IsItemSellable takes item whichItem returns boolean

native IsItemPawnable takes item whichItem returns boolean

native IsItemIdPowerup takes integer itemId returns boolean

native IsItemIdSellable takes integer itemId returns boolean

native IsItemIdPawnable takes integer itemId returns boolean

native EnumItemsInRect takes rect r, boolexpr filter, code actionFunc returns nothing

native GetItemLevel takes item whichItem returns integer

native GetItemType takes item whichItem returns itemtype

native SetItemDropID takes item whichItem, integer unitId returns nothing

/**
@async
*/
constant native GetItemName takes item whichItem returns string

native GetItemCharges takes item whichItem returns integer

native SetItemCharges takes item whichItem, integer charges returns nothing

native GetItemUserData takes item whichItem returns integer

native SetItemUserData takes item whichItem, integer data returns nothing

native BlzSetItemName                              takes item whichItem, string name returns nothing
native BlzSetItemDescription                       takes item whichItem, string description returns nothing
native BlzGetItemDescription                       takes item whichItem returns string
native BlzSetItemTooltip                           takes item whichItem, string tooltip returns nothing
native BlzGetItemTooltip                           takes item whichItem returns string
native BlzSetItemExtendedTooltip                   takes item whichItem, string extendedTooltip returns nothing
native BlzGetItemExtendedTooltip                   takes item whichItem returns string
native BlzSetItemIconPath                          takes item whichItem, string iconPath returns nothing
native BlzGetItemIconPath                          takes item whichItem returns string

