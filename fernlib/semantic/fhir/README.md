# FHIR

FHIR (Fern High-level Intermediate Representation) is the fully typed semantic IR the binder produces from the AST. It is a visitor based tree like the AST, however every expression node carries a resolved type and a backpointer to the syntax it came from, so any node can still map back to a source location for errors and IDE features.

## Poisoning 

When binding fails FHIR does not throw away the tree. It produces poison nodes that still wrap the original reference, so a broken file keeps enough structure for hover and goto definition to work, and one error does not cascade into many.

## Other

- Constant folding that computes an expression's value where one exists, used for integer range checks and constant conditions.
- A flow pass that checks every path returns a value and flags unreachable code.
- A hit test that finds the node and symbol at a line and column, to power IDE hover and goto definition.
- Formatters that print FHIR back as readable pseudo source or dump the full tree for debugging.
