// Boolean Expr API ( for compositing trigger conditions and unit filter funcs...)

/**
This function is used as and operator, but for boolexpr types.
So, it's same as (operandA and operandB) in code terms.
*/
native And              takes boolexpr operandA, boolexpr operandB returns boolexpr

/**
This function is used as or operator, but for boolexpr types.
So, it's same as (operandA or operandB) in code terms.
*/
native Or               takes boolexpr operandA, boolexpr operandB returns boolexpr

/**
This function is used as not operator, but for boolexpr types.
So, it's same as (not operand) in code terms.
*/
native Not              takes boolexpr operand returns boolexpr

native Condition        takes code func returns conditionfunc

/**
@note Only call this on conditionfuncs created via And,Or,Not.
*/
native DestroyCondition takes conditionfunc c returns nothing

native Filter           takes code func returns filterfunc

/**
@note Only call this on filterfunc created via And,Or,Not.
*/
native DestroyFilter    takes filterfunc f returns nothing

/**
@note Only call this on boolexpr created via And,Or,Not.
*/
native DestroyBoolExpr  takes boolexpr e returns nothing
