// Doodad API

/**
Makes doodads in the vicinity of a point play an animation.

@param x x-coordinate (world units) of the point.
@param y y-coordinate (world units) of the point.
@param radius Maximum pick distance from the point.
@param doodadID The type of the doodad that should be affected.
@param nearestOnly If true, only the single doodad (of the given type) closest to the point will be affected, otherwise all in the vicinity (of the given type).
@param animName String identifier of the animation that should be played.
@param animRandom If true, the animation to be played will be picked from an extended set including different variations of the animName, e.g., if animName is "walk", it can also be "walk defend".

@note Only doodads whose origin is within the radius distance of the point are considered.

@note There are the special values "hide" and "show" for animName, which will hide respectively show the doodad. When a doodad is hidden this way, its animation will pause at the current time frame. Re-showing the doodad resumes the animation.

@note If a target does not have an animation identified by animName (and it's not one of the special animation names either), it will play its first declared animation instead.

@bug If animName is null and there is at least one target, the game will crash.

@note If animRandom is true and the picked animation is looped, it will freshly re-pick from the set when an animation ends.
*/
native SetDoodadAnimation       takes real x, real y, real radius, integer doodadID, boolean nearestOnly, string animName, boolean animRandom returns nothing

/**
Makes doodads within a rect play an animation.

@param r The rect wherein doodads should play an animation.

@note Only doodads whose origin is in the rect are considered targets.

See SetDoodadAnimation for other parameters and notes.
*/
native SetDoodadAnimationRect   takes rect r, integer doodadID, string animName, boolean animRandom returns nothing
