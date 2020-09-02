# uilang

a weird language that tries to be optimized for creating uis in dom like things

## ideas

for the parser Generator
- consider more the possability of having a special thing that returns the remainder of the items in the union and replace magic with that
- consider could `csl[','] ','?` or `'|'? item['|']` be used instead of having `[',']` allow an extra last item? that would help with part 1 of this list of ideas
- eg `	| '|'? #['|']<or_op>` where `#` refers to the rest of the union
- eg `	| #[]<p_op>`
- eg `	| #suffix(suffixop)<suffixop>` ok actually idk how to suffixes because an important part is the unwrapping. it can definitely be done for the other two.