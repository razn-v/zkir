# zkIR

zkIR is a minimal IR for ZK circuits.

It provides a DSL-agnostic way to describe circuits in order to transpile them to multiple zkDSLs (Circom, Noir...).

# Goals

- Unified syntax for core zkDSLs features (constraints, inputs, outputs etc.)
- Transpilable to Circom, Noir and more
- Support for random AST generation
- Mutation and coverage support for cross-DSL fuzzing
- Support for gadgets shared among all zkDSLs
