// Image API

// <http://www.wc3c.net/showthread.php?t=107737>

/**
Creates the image.

@param file
The path to the image. The image itself should have its border alpha-ed out completely. If an invalid path is specified CreateImage returns image(-1).

@param sizeX
The x-dimensions of the image.

@param sizeY
The y-dimensions of the image.

@param sizeZ
The z-dimensions of the image.

@param posX
The x-cooridnate of where to create the image. This is the bottom left corner of the image.

@param posY
The y-cooridnate of where to create the image. This is the bottom left corner of the image.

@param posZ
The z-cooridnate of where to create the image.

@param originX
Moves the origin (bottom left corner) of the image from posX in negative X-direction.

@param originY
Moves the origin (bottom left corner) of the image from posY in negative Y-direction.

@param originZ
Moves the origin (bottom left corner) of the image from posZ in negative Z-direction.

@param imageType
Working values range from 1 to 4 (4 and 1 included). Using 0 causes CreateImage to return image(-1). Every other value will simply cause WC3 to not display the image.
imageTypes also influence the order in which images are drawn above one another:
imageType 1 (labeled by Blizzard with "Selection") is drawn above all other imageTypes.
imageType 2 (labeled by Blizzard with "Indicator") is drawn above imageType 4, but below 1 and 3.
imageType 3 (labeled by Blizzard with "Occlusion Mask") is drawn above imageType 4 and 2 and below imageType 1.
imageType 4 (labeled by Blizzard with "Ubersplat") is drawn below every other type. Images of this type are additionally affected by time of day and the fog of war (only for tinting).
Multiple images with the same type are drawn in their order of creation, meaning that the image created first is drawn below the image created after.

This returns a new image, the first ID given being 0 and then counting upwards (0, 1, 2, 3, ...).
*/
native CreateImage                  takes string file, real sizeX, real sizeY, real sizeZ, real posX, real posY, real posZ, real originX, real originY, real originZ, integer imageType returns image

/**
The destructor for images.

@param whichImage Which image to destroy.

This function destroys the image specified and recycles the handle ID of that image instantly (no ref counting for images).

@bug May crash the game if an invalid image is used (null, before the first image is created).
*/
native DestroyImage                 takes image whichImage returns nothing

/**
It shows/hides image whichImage, depending on boolean flag (true shows, false hides).
Seems like a redundant function in the light of SetImageRender(Always).
*/
native ShowImage                    takes image whichImage, boolean flag returns nothing

/**
Untested, but if its decription can account for anthing, it locks the Z position to the given height, if the flag is true.
After a bit of testing i concluded that this is the only function thats able to modify an images Z offset.
*/
native SetImageConstantHeight       takes image whichImage, boolean flag, real height returns nothing

/**
Sets the X/Y position of the provided image. This is the bottom left corner of the image, unless you used values form originX/Y/Z in the constructor other than 0, in which case the bottom left corner is moved further into negative X/Y/Z direction.
*/
native SetImagePosition             takes image whichImage, real x, real y, real z returns nothing

/**
Valid values for all channels range from 0 to 255.
*/
native SetImageColor                takes image whichImage, integer red, integer green, integer blue, integer alpha returns nothing

/**
@bug Does not work. Use SetImageRenderAlways instead.
*/
native SetImageRender               takes image whichImage, boolean flag returns nothing

/**
Since SetImageRender is non-functional, this should be used to enable/disable rendering of the image.
*/
native SetImageRenderAlways         takes image whichImage, boolean flag returns nothing

/**
Draws the specified image above the water if the flag is true. The second boolean (useWaterAlpha) doesnt seem to do much.
Every imagetype other than 1 doesnt seem to appear above water.
*/
native SetImageAboveWater           takes image whichImage, boolean flag, boolean useWaterAlpha returns nothing

/**
Changes the specified images type.

@param imageTypes also influence the order in which images are drawn above one another:
imageType 1 (labeled by Blizzard with "Selection") is drawn above all other imageTypes.
imageType 2 (labeled by Blizzard with "Indicator") is drawn above imageType 4, but below 1 and 3.
imageType 3 (labeled by Blizzard with "Occlusion Mask") is drawn above imageType 4 and 2 and below imageType 1.
imageType 4 (labeled by Blizzard with "Ubersplat") is drawn below every other type. Images of this type are additionally affected by time of day and the fog of war (only for tinting).
Multiple images with the same type are drawn in their order of creation, meaning that the image created first is drawn below the image created after.
*/
native SetImageType                 takes image whichImage, integer imageType returns nothing
