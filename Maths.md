Maths of the NNs
===

uppercase = vector, lowercase = single value / function
Inputs: I
Outputs: O
Parameters: P
Evaluation: e

Buildingblock B(I_b,P_b) as seen from its usage within an NN:

e = f(I,P,B(G(I,P),P))

where f describes everything after and around B and G is the function that describes the inputs of B as a function of the inputs and parameters. We assume that every parameter is used exactly once. That is, for each p in P exactly one of f, B and G depends on it.



