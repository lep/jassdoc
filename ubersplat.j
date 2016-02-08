// Ubersplat API

native CreateUbersplat              takes real x, real y, string name, integer red, integer green, integer blue, integer alpha, boolean forcePaused, boolean noBirthTime returns ubersplat

native DestroyUbersplat             takes ubersplat whichSplat returns nothing

/**
@bug Does nothing.
*/
native ResetUbersplat               takes ubersplat whichSplat returns nothing

/**
@bug Does nothing.
*/
native FinishUbersplat              takes ubersplat whichSplat returns nothing

native ShowUbersplat                takes ubersplat whichSplat, boolean flag returns nothing

native SetUbersplatRender           takes ubersplat whichSplat, boolean flag returns nothing

native SetUbersplatRenderAlways     takes ubersplat whichSplat, boolean flag returns nothing