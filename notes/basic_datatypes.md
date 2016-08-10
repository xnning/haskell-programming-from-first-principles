# Basic datatypes

Numeric types
  - Integral numbers
    - Int: fixed-precision; an artifact of what computer hardware has supported natively over the years.
    - Integer: arbitrarily large (or small) numbers,
  - Fractional
    - Float: single-precision floating point numbers.
      Violate some common assumptions and should only be used with great care.
    - Double: double-precision floating point numbers.
    - Rational: fractional number that represents a ratio of two integers; arbitrarily precise but not as efficient as Scientific.
    - Scientific: a space efficient and almost-arbitrary precision scientific number type; represented using scientific notation, with coefficient as an Integer and the exponent as an Int.

values of Fractional a => a default to the floating point type Double.

six categories of entities that have names.
  - term-level: variables, data constructors
  - type-level: type variables, type constructors, typeclasses
  - modules.
