Typeclasses
--------------

- Typeclasses and types in Haskell are, in a sense, opposites: where a declaration of a type defines how that type in particular is created, a declaration of a typeclass defines how a set of types are consumed or used in computations.

- When you have a typeclass-constrained (ad-hoc) polymorphic value and need to evaluate it, the polymorphism must be resolved to a specific concrete type. But in some cases, particularly when you’re working in the GHCi REPL you will not have specified a concrete type for a polymorphic value. In those situations, the typeclass will default to a concrete type, and the default types are already set in the libraries.

- The Haskell Report3 specifies the following defaults relevant to numerical computations:
  - default Num Integer
  - default Real Integer
  - default Enum Integer
  - default Integral Integer
  - default Fractional Double
  - default RealFrac Double
  - default Floating Double
  - default RealFloat Double

- The use of polymorphic values without the ability to infer a specific type and no default rule will cause GHC to complain about an ambiguous type.
