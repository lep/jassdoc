// Blight API

native SetBlight                takes player whichPlayer, real x, real y, real radius, boolean addBlight returns nothing

native SetBlightRect            takes player whichPlayer, rect r, boolean addBlight returns nothing

native SetBlightPoint           takes player whichPlayer, real x, real y, boolean addBlight returns nothing

native SetBlightLoc             takes player whichPlayer, location whichLocation, real radius, boolean addBlight returns nothing

/**
Creates a new, undead blighted gold mine unit at the specified coordinates for the player. The haunted gold mine will create blight around the area, and will become a normal gold mine when destroyed. The amount of gold in the mine is determined by the Data - Max Gold field for the ability Gold Mine ability ('Agld').

@param id
The player to create the goldmine for.

@param x
The x-coordinate of the goldmine.

@param y
The y-coordinate of the goldmine.

@param face
The facing of the goldmine in degrees.
*/
native CreateBlightedGoldmine   takes player id, real x, real y, real face returns unit

native IsPointBlighted          takes real x, real y returns boolean
