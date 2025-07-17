---Defines a new subtype using the game API.
---
---This is used to replace the Jass syntax: `type agent extends handle`
---with: `TypeDefine('agent', 'handle')`
---
---In contrast, natives do not need to be registered in Lua. They are exported automatically.
---@param newType string
---@param parentType string
function TypeDefine(newType, parentType)
	-- function TypeDefine is a lua C closure
end
