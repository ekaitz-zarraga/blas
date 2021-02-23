# Data representation

We are going to use a representation model based on Chicken Scheme's
you can read about it here:

<https://www.more-magic.net/posts/internals-data-representation.html>

This chapter itself is a **design decision**. We could have chosen any other
representation but this one fits very well in our case and it's pretty well
designed, as we'll see.

All our values are 32 bit and their types are encoded in the lower bits like
so:

| Type           |   Representation       |
+----------------+------------------------+
| Pointer        | `XX...XXXXXXXXXXXXX00` |
| Small integer  | `XX...XXXXXXXXXXXXXX1` |
| Other          | `XX...XXXXXXXXXXXXX10` |


## Direct access

### Pointer

Memory is 4-byte aligned because we are working with words. That means the 2
LSB are always 0 in pointers. We can use them to recognize the word as a
pointer without losing addressing space. This is **good design**. We don't lose
anything and we can recognize the word as a pointer.

### Small integer

Has the LSB set to 1.
We lose range, but 31 bits integers are still big enough for many things and we
can always define bignums as indirect data types later.

### Other

Other contains all data types that don't need the whole 32 bit word to work.
They include fixed literals like booleans and empty lists and also characters:

| Type           |   Representation       |
+----------------+------------------------+
| Boolean        | `XX...XXXXXXXXXXX0110` |
| Character      | `XX...XXXXXXXXXXX1010` |
| Special        | `XX...XXXXXXXXXXX1110` |


#### Booleans

In Chicken scheme booleans are represented like `000000...00X0110`, using only
one of the bits of the word for the value. We are going to use the whole space
to enable some crazy binary tricks like checking if the boolean is true or
false just checking if it's positive or negative. This is, of course, a
**design decision**.

| Type           |   Representation       |
+----------------+------------------------+
| True           | `00...000000000000110` |
| False          | `11...111111111110110` |

#### Characters

Chicken's characters only use the highest 24 bits, they add four padding 0s
between the type tag and the value for that. This enables cool encoding stuff
using some addressing tricks.

24 bits are available to represent the whole Unicode range, but if it grows for
any reason we could always use those four zeros too, sacrificing the alignment
tricks.

| Type           |   Representation       |
+----------------+------------------------+
| Character      | `XX...XXXXXXX00001010` |

#### Special

Special data are just constants that need to be used in Scheme programs like
`'()` and others.

| Type           |   Representation       |
+----------------+------------------------+
| Empty list     | `00...000000000001110` |
| Undefined      | `00...000000000011110` |
| Unbound var    | `00...000000000101110` |
| End of file    | `00...000000000111110` |

> In Guile, undefined can be made with `*unspecified*`, which is basically this
> nonsense: `(if #f #f)`, an expression that returns nothing.

## Indirect access

This is what we have once we followed a pointer. The pointer itself is going to
point to this header position.

Chicken uses 32 bits of header in the containers:

`XXXXYYYYSSSSSSSSSSSSSSSSSSSSSSSS`

- `X`: Garbage collection related data
- `Y`: Type (string, vector, pair...)
- `S`: Size of the contents

> DESIGN DECISION TO MAKE:  
> Chicken Scheme has a very special way to call functions and convert them to
> normal C functions, we may not be able to do that so we may need to create some
> indirect data type that is a function that has a data structure that finally
> has a pointer to the actual assembly code of the function. The data structure
> should have: the amount of input arguments the function consumes (to be able 
> reproduce `apply`?).


