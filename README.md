# uilang

a weird language that tries to be optimized for creating uis in dom like things

## ideas

for the parser Generator
- consider more the possability of having a special thing that returns the remainder of the items in the union and replace magic with that
- consider could `csl[','] ','?` or `'|'? item['|']` be used instead of having `[',']` allow an extra last item? that would help with part 1 of this list of ideas
- eg `	| '|'? #['|']<or_op>` where `#` refers to the rest of the union
- eg `	| #[]<p_op>`
- eg `	| #suffix(suffixop)<suffixop>` ok actually idk how to suffixes because an important part is the unwrapping. it can definitely be done for the other two.
- maybe a `<*>` eg `('|'? #['|']<*>)<or_op>` so it doesn't mape a p or something idk
- and make it so named single things get put into a struct like `file = a<b>` makes a struct with b but `file = a<>` makes no struct, same as `file = a<*>`.
- remove `<>` because it looks dumb. idk what to replace it with though
- disallow spaces inside things. eg `a ['b']` should not be allowed. also to do this, spaces need to be read as a token and not skipped. either this or have an autoformatter, this is probably easier and better.