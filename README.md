# Carton

An OCaml implementation of Olivier Carton's [*Langages formels, Calculabilité, Complexité*](https://gaati.org/bisson/tea/lfcc.pdf).

## Rational languages

The `lib/rational_language` folder is about the first chapter of the book, *Rational languages*, in the following order:

1. `words.ml` contains definitions and algorithms related to words, periods and morphisms. For instance, `guibas_odlyzko w` implements Guidas-Odlyzko's theorem by returning a word `w'` on the binary alphabet with the same periods of `w`.

2. `re.ml` defines rational expressions and implements a couple of algorithms related.

3. `auto.ml` defines the `auto` type and multiple functions to implement all possible NFAs. It mainly serves as an abstraction used for `auto_alg.ml`.

4. `auto_alg` defines many algorithms on automata.

