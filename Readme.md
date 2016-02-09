# Jass Doc

This goal of this project is not to document *every* function and native but
to add helpful comments for unclear functions, give notes about buggy functions
and inform about other misc informations.

I have split up the common.j in the same way the common.j was already
structured internally.

So far i have used `@param`, `@bug`, `@note`, `@pure` and `@event` annotations.
Other possible annotations could be `@synchronous`, `@nosideeffect`.

# Build

To build this project you need a somewhat recent GHC,
the haskell library megaparsec, the lexical analyser generator alex,
gnu make and the sqlite3 cli binary.

