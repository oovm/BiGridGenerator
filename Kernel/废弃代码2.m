
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

