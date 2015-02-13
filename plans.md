Plans for LMBS
==============

This document lays out the plans for creating LMBS.

Creating a phonology
--------------------

A phoneme list is created via

    (create-phoneme-list arg)

`arg` is a list that looks like this:

    ((ipa-string-1 class-string-1)
     (ipa-string-2 class-string-2)
     (ipa-string-3 class-string-3)
     ...
     (ipa-string-n class-string-n))

ipa-string is a string that has a valid IPA phoneme in it (not checked),
and class-string is either a character or a string with an identifier.
Probably just a character.

--------------------

Create a phonology by evaluating

    (create-quick-phonology prototype)

`prototype` is a language type that will influence how and what
phonemes might be chosen.

*Examples:*

* `:japonic`
  (few phonemes, strict syllable structure)
* `:germanic`
  (lots of vowels, loose syllable structure, lots of clustering)
* `:oceanic` or `:pacific`
  (very few phonemes, vowel clusters everywhere.)
* `:bantu` *Questionable?*
  (lots of phonemes, clicks and other exotic sounds)

Alternatively, you can have complete control over the creation by evaluating

    (create-phonology phoneme-count syllable-structure phonotactics)

Input arguments are obvious.

`phoneme-count` can either be `:few`, `:medium`, or `:lots`
for a qualitative analysis,
or an actual list of phonemes
that might be created by a `(create-phoneme-list IPA-plus-phoneme-class)`.
`syllable-structure` can be `:simple` or `:complex`
or maybe 
