# pipeline

`value»someFn().somethingElse(#) catch |e| another()`

the thing left of the pipeline is slotted in at the `#` to the right of the pipeline

this allows

`array»Array.map(#, …)»#.len`

`array»Array.map(#, |a| a + 1)»Array.reduce(#, number: 0, |t, a| t + a)`

`thingone»something() catch |e| #` // thingone will only be evaluated if the catch happens

# reverse pipeline

`someFn(#)«value`

not sure what this is for and it's not implemented yet
