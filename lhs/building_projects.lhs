Bulding projects
------------------------

Contents
========================

- writing Haskell programs with modules
- using the Cabal package manager
- building our project with Stack
- conventions around Haskell project organization
- building a small interactive game

Notes
========================

- The Haskell Cabal, or Common Architecture for Building Applications and Libraries, is a package manager.

- Stack is a cross-platform program for developing Haskell projects.
    - The stack.yaml file is used to determine the versions of your packages and what version of GHC they’ll work best with.

- Modules
    - The effect of multiple import declarations is cumulative, but the ordering of import declarations is irrelevant.

Exercises
========================

**Intermission: Check your understanding**

1. forever, when
2. Data.Bits, Database.Blacktip.Types.
3. the main data constructors used in blacktip.
4. code comparison
    a. Control.Concurrent.MVar; Filesystem.Path.CurrentOS; Control.Concurrent
    b. Filesystem
    c. Control.Monad
