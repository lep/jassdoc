// Trackable API

/**
Creates a trackable at the given coordinates but with zero z-coordinate.
Trackables are used to register mouse clicks or hovers at the trackables
position. But their functionality is very limited, as you can't, for example
distinguish the triggering player out of the box. To get a general overview
to the common workarounds see the `trackable` documentation.

@note To create a trackable with a non-zero z-coordinate you can use the same
technique as with `AddSpecialEffect`, that is create an invisible platform
before creating the trackable.

```
function CreateTrackableZ takes string trackableModelPath, real x, real y, real z, real facing returns trackable
    local destructable d = CreateDestructableZ('OTip', x, y, z, 0, 1, 0)
    local trackable t = CreateTrackable(trackableModelPath, x, y, facing)
    call RemoveDestructable(d)
    set d = null
    return t
endfunction
```

@param trackableModelPath The path to the model the trackable should use. Models
with team colours will use the neutral-hostile team colour. To create an
invisible trackable provide the empty string `""`.

@param x The x-coordinate where the trackable should be created

@param y The x-coordinate where the trackable should be created

@param facing The facing of the trackable

*/
native CreateTrackable      takes string trackableModelPath, real x, real y, real facing returns trackable
