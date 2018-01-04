
game24[input_List, result_: 24] :=
	Quiet@Block[{add, sub, mul, div},
		With[{oprules = {add -> Plus, sub -> Subtract, mul -> Times,
			div -> Divide},
			specifics = {div[x_, 1] :> x, mul[x_, 1] :> x, mul[1, x_] :> x,
				add[2, 2] -> mul[2, 2]}},
			ParallelMap[RightComposition[
				Hold, ReplaceAll[oprules], ToString[#, InputForm] &,
				StringDelete[{"Hold[", "]"}],
				StringReplace[{"*" -> "\[Times]", "/" -> "\[Divide]"}]
			],
				Union[
					Select[result == (# /. oprules) &]@
						Groupings[Permutations@input, {add, sub, mul, div} -> 2],
					SameTest -> (0 ===
						Simplify[
							sub[#1, #2] //. specifics /.
								Prepend[oprules, k_Integer :> ToString[k]]] &)
				]
			]
		]
	];

RootHypergeometric[n_Integer,m_,t_]:=Sum[
	(-(1/((n-1)*k!)))*t^k*E^((2*Pi*I*(k-1)*m)/(n-1))*
		Pochhammer[(k-1)/(n-1)+1,k-1]*HypergeometricPFQ[Range[n-1]/n+(k-1)/(n-1),
		Delete[Range[k+1,k+n-1],-k+n-1]/(n-1),n*((n*t)/(n-1))^(n-1)],{k,0,n-2}
];
Root[#1^5-#1+1&,2]//N
N@RootHypergeometric[5,1,1]//Chop