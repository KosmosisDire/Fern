# Binder

The binder walks the AST and converts it into the semantic representation: FHIR (Fern high-level intermediate representation). This is very similar to the binder in Rosyln conceptually. It is responsible for creating symbols for declarations, resolving names back to those symbols, inferring types, doing basica local type checking, and creating FHIR nodes with annotated types.

It is so big I ended up splitting the single class across multiple organized files. And unfortunetely I cannot see a good way to decouple most of the login within this class. Removing them from eachother makes it so much more complicated because we have to pass a bunch of state around anyways between them. Roslyn also does it this way, so I think that design is backed up pretty well.

