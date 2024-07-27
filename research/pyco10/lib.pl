% q with graph subsumption
%containing graph declares subsumption of subgraph.

q(S,P,O,G) :-
  q(S,P,O,X),
  q(X,is_part_of,G,G).

