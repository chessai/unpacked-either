# unpacked-either

This module is intended to be a drop-in replacement for Either. To shave off pointer chasing, it uses '-XUnboxedSums' to represent the Either type as two machine words that are contiguous in memory, without loss of expressiveness that Either provides.

This library provides pattern synonyms Left and Right that allow users to pattern match on an Unpacked Either in a familiar way.

Functions are also provided for converting an Unpacked Either to the base library's Either, and vice versa.

This library is in alpha, and the internals are likely to change.
