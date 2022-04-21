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

/**
Change(set) the item name at runtime.

@patch 1.29
@bug Doesn't work
*/
native BlzSetItemName                              takes item whichItem, string name returns nothing

/**
Change(set) the item description at runtime.

@patch 1.29
*/
native BlzSetItemDescription                       takes item whichItem, string description returns nothing

/**
Get the item description.

@async
@patch 1.29
*/
native BlzGetItemDescription                       takes item whichItem returns string


/**
Change(set) the item tooltip at runtime.

@patch 1.29
@bug Doesn't work
*/
native BlzSetItemTooltip                           takes item whichItem, string tooltip returns nothing

/**
Get the item tooltip.

@async
@patch 1.29
*/
native BlzGetItemTooltip                           takes item whichItem returns string



/**
Change(set) the extended item tooltip at runtime.

@patch 1.29
*/
native BlzSetItemExtendedTooltip                   takes item whichItem, string extendedTooltip returns nothing

/**
Get the extended item tooltip.

@async
@patch 1.29
*/
native BlzGetItemExtendedTooltip                   takes item whichItem returns string


/**
Change(set) the item icon path at runtime.

@patch 1.29
*/
native BlzSetItemIconPath                          takes item whichItem, string iconPath returns nothing

/**
Get the item icon path.

@patch 1.29
*/
native BlzGetItemIconPath                          takes item whichItem returns string


// Item 

/**
@patch 1.31
*/
native BlzGetItemAbilityByIndex                    takes item whichItem, integer index returns ability

/**
@patch 1.31
*/
native BlzGetItemAbility                           takes item whichItem, integer abilCode returns ability

/**
@note The item has to be carried by a unit for this to work.

@patch 1.31
*/
native BlzItemAddAbility                           takes item whichItem, integer abilCode returns boolean

/**
@patch 1.31
*/
native BlzGetItemBooleanField                      takes item whichItem, itembooleanfield whichField returns boolean

/**
@patch 1.31
*/
native BlzGetItemIntegerField                      takes item whichItem, itemintegerfield whichField returns integer

/**
@patch 1.31
*/
native BlzGetItemRealField                         takes item whichItem, itemrealfield whichField returns real

/**
@patch 1.31
*/
native BlzGetItemStringField                       takes item whichItem, itemstringfield whichField returns string

/**
@patch 1.31
*/
native BlzSetItemBooleanField                      takes item whichItem, itembooleanfield whichField, boolean value returns boolean

/**
@patch 1.31
*/
native BlzSetItemIntegerField                      takes item whichItem, itemintegerfield whichField, integer value returns boolean

/**
@patch 1.31
*/
native BlzSetItemRealField                         takes item whichItem, itemrealfield whichField, real value returns boolean

/**
@patch 1.31
*/
native BlzSetItemStringField                       takes item whichItem, itemstringfield whichField, string value returns boolean

/**
@patch 1.31
*/
native BlzItemRemoveAbility                        takes item whichItem, integer abilCode returns boolean
