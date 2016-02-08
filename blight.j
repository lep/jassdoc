// Blight API

native SetBlight                takes player whichPlayer, real x, real y, real radius, boolean addBlight returns nothing

native SetBlightRect            takes player whichPlayer, rect r, boolean addBlight returns nothing

native SetBlightPoint           takes player whichPlayer, real x, real y, boolean addBlight returns nothing

native SetBlightLoc             takes player whichPlayer, location whichLocation, real radius, boolean addBlight returns nothing

native CreateBlightedGoldmine   takes player id, real x, real y, real face returns unit

native IsPointBlighted          takes real x, real y returns boolean