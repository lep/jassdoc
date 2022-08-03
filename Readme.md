
# Jass Doc

The Jass Doc is an encyclopedia-like source for information about Jass natives.
The intended use is as a reference for understanding both the intended behavior
and the bugs present for wc3 natives and blizzard.j functions.

This goal of this project is not to document *every* function and native but
to add helpful comments for unclear functions, give notes about buggy functions
and inform about other misc information.

The jass natives are grouped into categories and split into files (e.g.
`unit.j` and `timer.j`). This project is structured like code so that it can be
compiled into one large document, but doing so is not necessary for making use
of this repository.

Currently there's [Jassbot - a JASS API search engine](https://lep.duckdns.org/app/jassbot/) running that uses this as a database.

Do note that the parser expects the annotations after the general description.

# How to add documentation

- You can register a Github account, go to any file and click "Edit file".
	- After you save changes, Github will ask you to create a Pull Request to submit your changes to us. Until then, you'll be editing your own copy of the files (you can't break anything)
- Send a message on [Hiveworkshop forums](https://www.hiveworkshop.com/threads/jassdoc.275521/)

Do not worry about 100% "quality". Many functions do not have any descriptions at all. If you can't describe a function fully, adding little hints to a description would help too!

# How to write annotations

The format roughly follows [Javadoc's approach](https://www.oracle.com/technical-resources/articles/java/javadoc-tool.html): The documentation is written as code comments above a function.

The doc generator supports *Markdown* syntax (the same that's used on Discord, Reddit, Github etc.) Here's [Reddit's simple explanation](https://old.reddit.com/wiki/markdown), though [currently this code library is used](https://python-markdown.github.io/).

For example a description of `CripplePlayer`:

```
/**
Reveals a player's remaining buildings to a force. The black mask over the
buildings will be removed as if the territory had been discovered

@note his function will not check whether the player has a town hall before revealing.

@param whichPlayer The player to reveal.

@param toWhichPlayers The players who will see whichPlayer's buildings.

@param flag If true, the buildings will be revealed. If false, the buildings
will not be revealed. Note that if you set it to false, it will not hide the buildings with a black mask.
*/
native CripplePlayer takes player whichPlayer, force toWhichPlayers, boolean flag returns nothing
```

You can start a description on the next line:

```
/**
@note one line. This word will be in **bold**
*/
```
```
/**
@note
Same as above, but on the next line, with a [link](http://example.com/function-discussion)
*/
```

# List of annotations

### Text without an @ annotation

Function's general description and explanation. This text must be at the start of the comment block.

### @param <variable Name>

Used to describe a function's variable. Add a short description, if the variable name doesn't properly explain its usage.

### @bug <text>

Describes buggy behaviour, when something doesn't work.

### @note <text>

Adds your note that doesn't belong to a general description. You can recommend alternatives, explain something etc.

### @pure

The function behaves like a [mathematical](https://wiki.haskell.org/Pure) `f(x) = x+123` function: it doesn't change any state outside (no side-effects), only uses the arguments provided (only `x` here; no globals) and may return a new value, like `x+123`. Calling this function again and again would return the same value.

### @async

Adds a separate note, explaining that the values returned by this function are *not* synchronized between players by the game. I.e. each player has their own value; using this to affect the game world will cause a desync.

### @event <EVENT_NAME>

 Lists events that the function interacts with. For example, `GetEnteringUnit()` only works together with `EVENT_GAME_ENTER_REGION`.

### @patch <patch vesion>

Describes when the function was introduced to the game. Many natives (game API) were added in `1.29`.

### Undocumented:

[Internal](https://github.com/lep/jassdoc-browser/blob/6b6799e90bad48a4ceb9a15b2bfe92c5f02a0e15/server.hs#L41): `@source-file`, `@source-code`, `@return-type`

Proposed: `@nosideeffect` for functions without side-effects.

# Build

To build this project you need a somewhat recent GHC,
the haskell library megaparsec-7, gnu make and the sqlite3 cli binary.

