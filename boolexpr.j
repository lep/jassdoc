// Boolean Expr API ( for compositing trigger conditions and unit filter funcs...)

/**
Returns a new boolean expression that has the result of evaluating logical (expr1 AND expr2)

@note `boolexpr` extends from `agent` and must be explicitly destroyed with `DestroyBoolExpr` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.
*/
native And              takes boolexpr operandA, boolexpr operandB returns boolexpr

/**
Returns a new boolean expression that has the result of evaluating logical (expr1 OR expr2)

@note `boolexpr` extends from `agent` and must be explicitly destroyed with `DestroyBoolExpr` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.
*/
native Or               takes boolexpr operandA, boolexpr operandB returns boolexpr

/**
Returns a new boolean expression that has the result of evaluating logical (NOT expr1).

@note `boolexpr` extends from `agent` and must be explicitly destroyed with `DestroyBoolExpr` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.
*/
native Not              takes boolexpr operand returns boolexpr

/**
Returns a new conditionfunc that has the result of evaluating func(). func will receive no arguments and must return a boolean: true/false.

@note 1.32.10, Lua: `conditionfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed with `DestroyBoolExpr`/`DestroyCondition` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note (Old description) Do not destroy conditionfuncs created with `Condition` because this function
does not create a new handle (`Condition(function foo) == Condition(function foo)`).
In the best case it does nothing but in the worst case it affects some internals.

@pure
*/
native Condition        takes code func returns conditionfunc

/**
Destroys the provided condition.

@note `conditionfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note Only call this on conditionfuncs created via `And`,`Or`,`Not`.
*/
native DestroyCondition takes conditionfunc c returns nothing

/**
Returns a new filterfunc that has the result of evaluating func().
func will receive no arguments and must return a boolean: true/false.

@note Lua, 1.32.10: `filterfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed with `DestroyBoolExpr`/`DestroyFilter` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note (Old description)Do not destroy filterfuncs created with `Filter` because this function
does not create a new handle (`Filter(function foo) == Filter(function foo)`).
In the best case it does nothing but in the worst case it affects some internals.

@pure
*/
native Filter           takes code func returns filterfunc

/**
Destroys the provided filter function.

@note `filterfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed with `DestroyBoolExpr`/`DestroyFilter` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note Only call this on filterfunc created via `And`,`Or`,`Not`.

@note See: `Filter`
*/
native DestroyFilter    takes filterfunc f returns nothing

/**
destroys the provided boolean expression

@note `boolexpr` extends from `agent` and must be explicitly destroyed to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note Only call this on boolexpr created via `And`,`Or`,`Not`.

@note See: `And`, `Or`, `Not`, `Condition`, `Filter`
*/
native DestroyBoolExpr  takes boolexpr e returns nothing
