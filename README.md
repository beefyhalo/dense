# dense: Type-Level Dense Binary Positional Numbering System

This project is an implementation of [Okasaki's Dense Binary Number System][okasaki]. It's simply a type-level linked-list of bits; a sort of HList of binary Digits. Included are the standard math operations you'd expect for natural numbers.

This library can be used alternatively to the Church-encoded Nat type found in [shapeless][shapeless].


[okasaki]: https://books.google.ca/books?id=SxPzSTcTalAC&lpg=PA116&ots=DGm_VDDnYU&dq=okasaki%20positional%20number%20dense&pg=PA117#v=onepage&q=okasaki%20positional%20number%20dense&f=false
[shapeless]: https://github.com/milessabin/shapeless