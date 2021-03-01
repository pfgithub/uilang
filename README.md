# uilang

a weird language that tries to be optimized for creating uis in dom like things

`zig 0.8.0-dev.1342+4f11a88b9`

## ideas

for the parser Generator

- consider more the possability of having a special thing that returns the remainder of the items in the union and replace magic with that
- consider could `csl[','] ','?` or `'|'? item['|']` be used instead of having `[',']` allow an extra last item? that would help with part 1 of this list of ideas
- eg ` | '|'? #['|']<or_op>` where `#` refers to the rest of the union
- eg ` | #[]<p_op>`
- eg ` | #suffix(suffixop)<suffixop>` ok actually idk how to suffixes because an important part is the unwrapping. it can definitely be done for the other two.
- maybe a `<*>` eg `('|'? #['|']<*>)<or_op>` so it doesn't mape a p or something idk
- ~~and make it so named single things get put into a struct like `file = a<b>` makes a struct with b but `file = a<>` makes no struct, same as `file = a<*>`~~. This cannot be done. To ensure a struct, maybe add a `.` or something that forces something to be a struct without adding an extra field.
- remove `<>` because it looks dumb. idk what to replace it with though
- disallow spaces inside things. eg `a ['b']` should not be allowed. also to do this, spaces need to be read as a token and not skipped. either this or have an autoformatter, this is probably easier and better.
- maybe rather than putting positions randomly where they fit, try wrapping all top level things with `struct {pos: (start, end), value: _12}` eg
- instead of "demo types", how about a function `T(comptime a: type, comptime b: type) type {return b}` and then instead of a: \_12 do a: T(struct {actual type}, \_12)

## goals

- building consistent2.ul
- writing/copying the js header stuff needed to make the output code useful (the code I have written already for a previous project does lists and html elements and fragments and watchables that update up I think and a bunch of stuff)
- eg if you sort and filter an array, adding new items to the array should not require resorting and refiltering and then diffing but should instead flag the array itself to insert items sorted and filtered.
- figure out what happens with recursive structures. figure out what happens with recursive watches (I think the answer here is that recursive watches don't exist because watch decls have their default value executed in fn context rather than module context)
- zig stuff:
  - async
  - structs and stuff and optionals and errors and whatever. preferably backed by arrays rather than objects with named keys. or maybe there can be a build option to do named keys for easier debug, but arrays are preferred
  - comptime
- inferred unions. eg `union {a: i53, b: struct {}}` and then `@as(that union, 25)` should automatically do .a. but if there is a conflict and the item you are coercing to the union can be coerced to more than one of the possible values, error and make you manually specify. (btw unions are union(enum) by default and also accessing fields can return optionals because zig should do that but can't because of the difference between `union` and `union(enum)`)
- idk other things make the language first

## zen

just the zig zen with n/a parts removed

- Communicate intent precisely.
- Edge cases matter.
- Favor reading code over writing code.
- Only one obvious way to do things.
- Runtime crashes are better than bugs.
- Compile errors are better than runtime crashes.
- Incremental improvements.
- Avoid local maximums. (this will be difficult because rn the language will compile to js. it shouldn't compile to js forever, but that depends if I get the language to a usable point ever or not)
- Reduce the amount one must remember.
- Focus on code rather than style.
- Resource allocation may fail; resource deallocation must succeed.
- ~~Memory is a resource.~~ Memory is infinite and will never end. If you run out, just download more.
- Together we serve the users.
