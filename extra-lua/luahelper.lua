           
-- Jass2 array that returns an empty default value when the key does not exist
--- Create and return a table with an __index metatable that returns the default value as fallback
---@param default any fallback value to replace nil
---@return table a new table
function __jarray(default)
    return setmetatable({}, {
        __index = function()
            return default
        end
    })
end

-- Math random functions should come from the game engine
--- Replaced by luahelper to call game API to set the seed
---@see SetRandomSeed
---@param seed integer
function math.randomseed(seed)
    return SetRandomSeed(seed // 1)
end

--- Replaced by luahelper to call game API instead
---@see GetRandomInt, GetRandomReal, math.random in Lua
---@param m integer
---@param n integer
---@return number|integer
---@overload fun(max: integer)
---@overload fun()
function math.random(m, n)
    if m and n then
        return GetRandomInt(m // 1, n // 1)
    elseif m then
        return GetRandomInt(1, m // 1)
    else
        return GetRandomReal(0.0, 1.0)
    end
end

if DisplayTextToPlayer then
    --- Replaced by luahelper to call game API instead
    --- Displays text to everyone.
    ---@see DisplayTextToPlayer
    ---@vararg string|any
    function print(...)
        local sb = {}
        for i = 1, select('#', ...) do
            sb[i] = tostring(select(i, ...))
        end
        DisplayTextToPlayer(GetLocalPlayer(), 0, 0, table.concat(sb, '    '))
    end
end

-- Helper function that enables the following syntax: ARCHMAGE = FourCC('Hamg')
--- Converts a four-letter string (aka rawcode) to a corresponding 32-bit integer value.  
--- **Bug:** it only converts 7-bit letters correctly. Map protection that uses characters 128-255
--- will have incorrectly converted FourCC codes (the game has per-character byte overflow).  
--- Only available in Lua. Jass compiler simply uses 'ABCD' syntax.
---@param id string 4-letter rawcode (use `string.byte` for 1-letter conversion)
---@return integer
function FourCC(id)
    return  0x1000000 * string.byte(id:sub(1,1)) +
              0x10000 * string.byte(id:sub(2,2)) +
                0x100 * string.byte(id:sub(3,3)) +
                        string.byte(id:sub(4,4))
end

-- Debug build?
--- Only internal debug builds of the game have this set to `true`.
--- This is used by Blizzard's jass2lua transpiler to handle the "debug" keyword of Jass (it excludes an entire statement that follows).
--- Example: "PlayDialogueFromSpeakerEx" in Blizzard.j
---
--- Discussion: <https://www.hiveworkshop.com/threads/blizzards-hidden-jass2lua-transpiler.337281/#post-3560384>
_DEBUG = false
