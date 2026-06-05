# Binder

Turns the AST into FHIR (Fern High-level IR). This resolves names, does types checking, and every expression comes out with a resolved symbol and type.


The scope binders form a chain, one node per lexical scope: block, method, type, namespace, root. Name lookup walks the chain from the innermost scope outward and takes the first match.

The chain splits into two roles:

- The main binder (in `core/`) holds the actual binding logic and the scopes inherit from it.
- The scope binders (in `scopes/`) are binders which provide specialized functions for looking up names within their scope. They are very simple. The point is that we no longer have to worry about tracking or passing scope context down the tree to change how we look stuff up in different scopes


