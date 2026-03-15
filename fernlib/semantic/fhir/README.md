# FHIR

This is the Fern High-level Intermediate Representation. It is a pure semantic representation of the code, which has been type resolved etc. So any construct that has multiple syntax representations will be normalized to a single semantic represenation here. It is also meant to be transformed unlike the AST. Most of the most basic desugaring transformations are just built into the binder: things like converting initializer lists into assignment statements. 


I have implemented a single transformation for constant folding, really more as a proof of concept than anything. Later there will be passes for things like closures, implicit casts, async/await state machines, etc.